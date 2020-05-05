package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{Actor, ActorRef, Cancellable, Status, typed}
import akka.pattern.{ask, pipe}
import akka.{actor => classic}
import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.Group
import cats.syntax.group.{catsSyntaxGroup, catsSyntaxSemigroup}
import com.wavesplatform.dex.AddressActor._
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.api.CanNotPersist
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Denormalization.denormalizeAmountAndFee
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError, UnexpectedError, WavesNodeConnectionBroken}
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalance
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.market.{BatchOrderCancelActor, CreateExchangeTransactionActor}
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCancelFailed, OrderCanceled, OrderExecuted, Event => OrderEvent}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent
import com.wavesplatform.dex.time.Time
import org.slf4j.LoggerFactory

import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap}
import scala.concurrent.duration._
import scala.concurrent.{Future, TimeoutException}
import scala.util.{Failure, Success}

class AddressActor(owner: Address,
                   time: Time,
                   orderDB: OrderDB,
                   validate: (AcceptedOrder, Map[Asset, Long]) => Future[Either[MatcherError, Unit]],
                   store: StoreEvent,
                   var enableSchedules: Boolean,
                   spendableBalancesActor: ActorRef,
                   settings: AddressActor.Settings = AddressActor.Settings.default)(implicit efc: ErrorFormatterContext)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))

  private var placementQueue  = Queue.empty[Order.Id]
  private val pendingCommands = MutableMap.empty[Order.Id, PendingCommand]

  private val activeOrders = MutableMap.empty[Order.Id, AcceptedOrder]
  private var openVolume   = Map.empty[Asset, Long]
  private val expiration   = MutableMap.empty[ByteStr, Cancellable]

  private var addressWsMutableState = AddressWsMutableState.empty

  override def receive: Receive = {
    case command: Command.PlaceOrder =>
      log.debug(s"Got $command")
      val orderId = command.order.id()

      if (totalActiveOrders >= settings.maxActiveOrders) sender ! api.OrderRejected(error.ActiveOrdersLimitReached(settings.maxActiveOrders))
      if (hasOrder(orderId)) sender ! api.OrderRejected(error.OrderDuplicate(orderId))
      else {
        val shouldProcess = placementQueue.isEmpty
        placementQueue = placementQueue.enqueue(orderId)
        pendingCommands.put(orderId, PendingCommand(command, sender))
        if (shouldProcess) processNextPlacement()
        else log.trace(s"${placementQueue.headOption} is processing, moving $orderId to the queue")
      }

    case command: Command.CancelOrder =>
      import command.orderId
      log.trace(s"Got $command")
      pendingCommands.get(orderId) match {
        case Some(pc) =>
          sender ! api.OrderCancelRejected(
            pc.command match {
              case _: Command.PlaceOrder  => error.OrderNotFound(orderId)
              case _: Command.CancelOrder => error.OrderCanceled(orderId)
            }
          )

        case None =>
          activeOrders.get(orderId) match {
            case None =>
              sender ! api.OrderCancelRejected(
                orderDB.status(orderId) match {
                  case OrderStatus.NotFound     => error.OrderNotFound(orderId)
                  case _: OrderStatus.Cancelled => error.OrderCanceled(orderId)
                  case _: OrderStatus.Filled    => error.OrderFull(orderId)
                }
              )

            case Some(ao) =>
              if (ao.isMarket) sender ! api.OrderCancelRejected(error.MarketOrderCancel(orderId))
              else {
                pendingCommands.put(orderId, PendingCommand(command, sender))
                cancel(ao.order)
              }
          }
      }

    case command: Command.CancelAllOrders =>
      val toCancelIds = getActiveLimitOrders(command.pair).map(_.id)
      if (toCancelIds.isEmpty) {
        log.trace(s"Got $command, nothing to cancel")
        sender ! api.BatchCancelCompleted(Map.empty)
      } else {
        log.debug(s"Got $command, to cancel: ${toCancelIds.mkString(", ")}")
        context.actorOf(BatchOrderCancelActor.props(toCancelIds.toSet, self, sender, settings.batchCancelTimeout))
      }

    case command: Command.CancelOrders =>
      val allActiveOrderIds = getActiveLimitOrders(None).map(_.order.id()).toSet
      val toCancelIds       = allActiveOrderIds.intersect(command.orderIds)
      val unknownIds        = command.orderIds -- allActiveOrderIds

      log.debug(
        s"Got $command, total orders: ${allActiveOrderIds.size}, to cancel (${toCancelIds.size}): ${toCancelIds
          .mkString(", ")}, unknown ids (${unknownIds.size}): ${unknownIds.mkString(", ")}"
      )

      val initResponse = unknownIds.map(id => id -> api.OrderCancelRejected(error.OrderNotFound(id))).toMap
      if (toCancelIds.isEmpty) sender ! api.BatchCancelCompleted(initResponse)
      else context.actorOf(BatchOrderCancelActor.props(toCancelIds, self, sender, settings.batchCancelTimeout, initResponse))

    case command: Command.CancelNotEnoughCoinsOrders =>
      if (addressWsMutableState.hasActiveConnections) {
        addressWsMutableState = addressWsMutableState.putSpendableAssets(command.newBalance.keySet)
      }

      val toCancel = getOrdersToCancel(command.newBalance).filterNot(ao => isCancelling(ao.order.id()))

      if (toCancel.isEmpty) log.trace(s"Got $command, nothing to cancel")
      else {
        val msg = toCancel
          .map(x => s"${x.insufficientAmount} ${x.assetId} for ${x.order.idStr()}")
          .mkString(", ")
        log.debug(s"Got $command, canceling ${toCancel.size} of ${activeOrders.size}: doesn't have $msg")
        toCancel.foreach(x => cancel(x.order))
      }

    case Query.GetReservedBalance            => sender ! Reply.Balance(openVolume.filter(_._2 > 0))
    case Query.GetTradableBalance(forAssets) => getTradableBalance(forAssets).map(xs => Reply.Balance(xs.filter(_._2 > 0))).pipeTo(sender)

    case Query.GetOrderStatus(orderId) => sender ! activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(activeStatus)
    case Query.GetOrdersStatuses(maybePair, orderListType) =>
      val matchingActiveOrders =
        if (orderListType.hasActive)
          getActiveLimitOrders(maybePair)
            .map(ao => ao.id -> OrderInfo.v4(ao, activeStatus(ao)))
            .toSeq
            .sorted
        else Seq.empty

      val matchingClosedOrders = if (orderListType.hasClosed) orderDB.getFinalizedOrders(owner, maybePair) else Seq.empty
      sender ! Reply.OrdersStatuses(matchingActiveOrders ++ matchingClosedOrders)

    case storeFailed @ Event.StoreFailed(orderId, reason, queueEvent) =>
      log.trace(s"Got $storeFailed")
      pendingCommands.remove(orderId).foreach { _.client ! CanNotPersist(reason) }
      queueEvent match {
        case QueueEvent.Placed(_) | QueueEvent.PlacedMarket(_) =>
          activeOrders.remove(orderId).foreach { ao =>
            openVolume = openVolume |-| ao.reservableBalance
            if (addressWsMutableState.hasActiveConnections)
              addressWsMutableState = addressWsMutableState.putReservedAssets(ao.reservableBalance.keySet)
          }
        case _ =>
      }

    case event: ValidationEvent =>
      log.trace(s"Got $event")
      placementQueue.dequeueOption.foreach {
        case (orderId, restQueue) =>
          if (orderId == event.orderId) {
            event match {
              case Event.ValidationPassed(ao) => pendingCommands.get(ao.id).foreach(_ => place(ao))
              case Event.ValidationFailed(_, reason) =>
                pendingCommands.remove(orderId).foreach { command =>
                  log.trace(s"Confirming command for $orderId")
                  command.client ! (
                    reason match {
                      case WavesNodeConnectionBroken => api.WavesNodeUnavailable(reason)
                      case _                         => api.OrderRejected(reason)
                    }
                  )
                }
            }

            placementQueue = restQueue
            processNextPlacement()
          } else log.warn(s"Received stale $event for $orderId")
      }

    case event @ OrderAdded(submitted, _) =>
      import submitted.order
      log.trace(s"OrderAdded(${order.id()})")
      refreshOrderState(submitted, event)
      pendingCommands.remove(order.id()).foreach { command =>
        log.trace(s"Confirming placement for ${order.id()}")
        command.client ! api.OrderAccepted(order)
      }

    case event: OrderExecuted =>
      log.trace(s"OrderExecuted(${event.submittedRemaining.id}, ${event.counterRemaining.id}), amount=${event.executedAmount}")
      List(event.submittedRemaining, event.counterRemaining).filter(_.order.sender.toAddress == owner).foreach(refreshOrderState(_, event))
      context.system.eventStream.publish(CreateExchangeTransactionActor.OrderExecutedObserved(owner, event))

    case event @ OrderCanceled(ao, isSystemCancel, _) =>
      val id       = ao.id
      val isActive = activeOrders.contains(id)
      log.trace(s"OrderCanceled($id, system=$isSystemCancel, isActive=$isActive)")
      if (isActive) refreshOrderState(ao, event)
      pendingCommands.remove(id).foreach { pc =>
        log.trace(s"Confirming cancellation for $id")
        pc.client ! api.OrderCanceled(id)
      }

    case OrderCancelFailed(id, reason) =>
      pendingCommands.remove(id).foreach { pc =>
        log.warn(s"Storing of cancel event for $id failed, reason: ${reason.message.text}")
        pc.client ! api.OrderCancelRejected(reason)
      }

    case CancelExpiredOrder(id) =>
      expiration.remove(id)
      activeOrders.get(id).foreach { ao =>
        if ((ao.order.expiration - time.correctedTime()).max(0L).millis <= ExpirationThreshold) {
          log.debug(s"Order $id expired, storing cancel event")
          cancel(ao.order)
        } else scheduleExpiration(ao.order)
      }

    case AddressDirectory.StartSchedules =>
      if (!enableSchedules) {
        enableSchedules = true
        activeOrders.values.foreach(x => scheduleExpiration(x.order))
      }

    case Status.Failure(e) => log.error(s"Got $e", e)

    case WsCommand.AddWsSubscription(client) =>
      log.trace(s"[${client.path.name}] WebSocket connected")
      spendableBalancesActor ! SpendableBalancesActor.Query.GetSnapshot(owner)
      addressWsMutableState = addressWsMutableState.addPendingSubscription(client)
      context.watch(client)

    case SpendableBalancesActor.Reply.GetSnapshot(allAssetsSpendableBalance) =>
      addressWsMutableState.sendSnapshot(
        balances = mkWsBalances(allAssetsSpendableBalance),
        orders = activeOrders.values.map(ao => WsOrder.fromDomain(ao, activeStatus(ao)))(collection.breakOut),
      )

      if (!addressWsMutableState.hasActiveConnections) scheduleNextDiffSending
      addressWsMutableState = addressWsMutableState.flushPendingConnections()

    case WsCommand.PrepareDiffForWsSubscribers =>
      if (addressWsMutableState.hasActiveConnections) {
        if (addressWsMutableState.hasChanges) {
          spendableBalancesActor ! SpendableBalancesActor.Query.GetState(owner, addressWsMutableState.getAllChangedAssets)
        } else scheduleNextDiffSending
      }

    case SpendableBalancesActor.Reply.GetState(spendableBalances) =>
      if (addressWsMutableState.hasActiveConnections) {
        addressWsMutableState = addressWsMutableState.sendDiffs(
          balances = mkWsBalances(spendableBalances),
          orders = addressWsMutableState.getAllOrderChanges
        )
        scheduleNextDiffSending
      }

      addressWsMutableState = addressWsMutableState.cleanChanges()

    case classic.Terminated(wsSource) => addressWsMutableState = addressWsMutableState.removeSubscription(wsSource)
  }

  private def scheduleNextDiffSending: Cancellable = {
    context.system.scheduler.scheduleOnce(settings.wsMessagesInterval, self, WsCommand.PrepareDiffForWsSubscribers)
  }

  private def mkWsBalances(spendableBalances: Map[Asset, Long]): Map[Asset, WsBalances] = {
    val tradableBalance = spendableBalances |-| openVolume.filterKeys(spendableBalances.keySet)
    spendableBalances.keySet.map { asset =>
      val balanceValue: Map[Asset, Long] => Double = source => denormalizeAmountAndFee(source.getOrElse(asset, 0), efc assetDecimals asset).toDouble
      asset -> WsBalances(balanceValue(tradableBalance), balanceValue(openVolume))
    }(collection.breakOut)
  }

  private def isCancelling(id: Order.Id): Boolean = pendingCommands.get(id).exists(_.command.isInstanceOf[Command.CancelOrder])

  private def processNextPlacement(): Unit = placementQueue.dequeueOption.foreach {
    case (firstOrderId, _) =>
      pendingCommands.get(firstOrderId) match {
        case None =>
          throw new IllegalStateException(
            s"Can't find command for order $firstOrderId among pending commands: ${pendingCommands.keySet.mkString(", ")}"
          )
        case Some(nextCommand) =>
          nextCommand.command match {
            case command: Command.PlaceOrder =>
              val validationResult = for {
                tradableBalance <- getTradableBalance(Set(command.order.getSpendAssetId, command.order.feeAsset))
                ao = command.toAcceptedOrder(tradableBalance)
                r <- validate(ao, tradableBalance)
              } yield
                r match {
                  case Left(error) => Event.ValidationFailed(ao.id, error)
                  case Right(_)    => Event.ValidationPassed(ao)
                }

              validationResult recover {
                case ex: WavesNodeConnectionLostException =>
                  log.error("Waves Node connection lost", ex)
                  Event.ValidationFailed(command.order.id(), WavesNodeConnectionBroken)
                case ex =>
                  log.error("An unexpected error occurred", ex)
                  Event.ValidationFailed(command.order.id(), UnexpectedError)
              } pipeTo self

            case x => throw new IllegalStateException(s"Can't process $x, only PlaceOrder is allowed")
          }
      }
  }

  private def getTradableBalance(forAssets: Set[Asset])(implicit group: Group[Map[Asset, Long]]): Future[Map[Asset, Long]] = {
    spendableBalancesActor
      .ask(SpendableBalancesActor.Query.GetState(owner, forAssets))(5.seconds, self) // TODO replace ask pattern by better solution
      .mapTo[SpendableBalancesActor.Reply.GetState]
      .map(xs => (xs.state |-| openVolume.filterKeys(forAssets)).withDefaultValue(0L))
  }

  private def scheduleExpiration(order: Order): Unit = if (enableSchedules && !expiration.contains(order.id())) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def refreshOrderState(remaining: AcceptedOrder, event: OrderEvent): Unit = {

    val origActiveOrder = activeOrders.get(remaining.id)

    lazy val origReservableBalance = origActiveOrder.fold(Map.empty[Asset, Long])(_.reservableBalance)
    lazy val openVolumeDiff        = remaining.reservableBalance |-| origReservableBalance

    val (status, isSystemCancel) = event match {
      case _: OrderAdded        => (activeStatus(remaining), false)
      case event: OrderCanceled => (OrderStatus.finalCancelStatus(remaining, event.isSystemCancel), event.isSystemCancel)
      case _: OrderExecuted =>
        val r =
          if (remaining.isValid) activeStatus(remaining)
          else OrderStatus.Filled(remaining.fillingInfo.filledAmount, remaining.fillingInfo.filledFee)
        (r, false)
    }

    log.trace(s"New status of ${remaining.id} is $status")

    status match {
      case status: OrderStatus.Final =>
        expiration.remove(remaining.id).foreach(_.cancel())
        activeOrders.remove(remaining.id).foreach(ao => openVolume = openVolume |-| ao.reservableBalance)
        orderDB.saveOrderInfo(remaining.id, owner, OrderInfo.v4(remaining, status))

      case _ =>
        activeOrders.put(remaining.id, remaining)
        openVolume = openVolume |+| openVolumeDiff
        status match {
          case OrderStatus.Accepted =>
            orderDB.saveOrder(remaining.order)
            scheduleExpiration(remaining.order)
          case _ =>
        }
    }

    if (addressWsMutableState.hasActiveConnections) {
      // OrderExecuted event and ExchangeTransaction creation are separated in time!
      // We should notify SpendableBalanceActor about balances changing, otherwise WS subscribers
      // will receive balance changes (its reduction as a result of order partial execution) with
      // sensible lag (only after exchange transaction will be put in UTX pool). The increase in
      // the balance will be sent to subscribers after this tx will be forged

      if (openVolumeDiff.nonEmpty) {
        val correction = Group.inverse(openVolumeDiff)
        spendableBalancesActor ! SpendableBalancesActor.Command.Subtract(owner, correction)
      }

      // Further improvements will be made in DEX-467
      addressWsMutableState = status match {
        case OrderStatus.Accepted     => addressWsMutableState.putOrderUpdate(remaining.id, WsOrder.fromDomain(remaining, status))
        case _: OrderStatus.Cancelled => addressWsMutableState.putOrderStatusUpdate(remaining.id, status)
        case _ =>
          if (isSystemCancel) addressWsMutableState.putOrderStatusUpdate(remaining.id, status)
          else addressWsMutableState.putOrderFillingInfoAndStatusUpdate(remaining, status)
      }

      // We already notified clients about these changes after order passed validation (see def place)
      if (status != OrderStatus.Accepted) addressWsMutableState = addressWsMutableState.putReservedAssets(openVolumeDiff.keySet)
    }
  }

  private def getOrdersToCancel(actualBalance: Map[Asset, Long]): Queue[InsufficientBalanceOrder] = {
    // Now a user can have 100 active transaction maximum - easy to traverse.
    activeOrders.values.toVector
      .sortBy(_.order.timestamp)(Ordering[Long]) // Will cancel newest orders first
      .iterator
      .filter(_.isLimit)
      .map(ao => (ao.order, ao.requiredBalance filterKeys actualBalance.contains))
      .foldLeft((actualBalance, Queue.empty[InsufficientBalanceOrder])) {
        case ((restBalance, toDelete), (order, requiredBalance)) =>
          trySubtract(restBalance, requiredBalance) match {
            case Right(updatedRestBalance) => (updatedRestBalance, toDelete)
            case Left((insufficientAmount, assetId)) =>
              val updatedToDelete =
                if (cancellationInProgress(order.id())) toDelete
                else toDelete.enqueue(InsufficientBalanceOrder(order, -insufficientAmount, assetId))
              (restBalance, updatedToDelete)
          }
      }
      ._2
  }

  private def cancellationInProgress(id: Order.Id): Boolean = pendingCommands.get(id).fold(false) {
    _.command match {
      case Command.CancelOrder(`id`) => true
      case _                         => false
    }
  }

  private def place(ao: AcceptedOrder): Unit = {
    openVolume = openVolume |+| ao.reservableBalance
    activeOrders.put(ao.id, ao)

    if (addressWsMutableState.hasActiveConnections) addressWsMutableState = addressWsMutableState.putReservedAssets(ao.reservableBalance.keySet)

    storeEvent(ao.id)(
      ao match {
        case ao: LimitOrder  => QueueEvent.Placed(ao)
        case ao: MarketOrder => QueueEvent.PlacedMarket(ao)
      }
    )
  }

  private def cancel(o: Order): Unit = storeEvent(o.id())(QueueEvent.Canceled(o.assetPair, o.id()))

  private def storeEvent(orderId: Order.Id)(event: QueueEvent): Unit =
    store(event)
      .transform {
        case Success(None) => Success(Some(error.FeatureDisabled))
        case Success(_)    => Success(None)
        case Failure(e) =>
          e match {
            case _: TimeoutException => log.warn(s"Timeout during storing $event for $orderId")
            case _                   =>
          }
          Success(Some(error.CanNotPersistEvent))
      }
      .onComplete {
        case Success(Some(error)) => self ! Event.StoreFailed(orderId, error, event)
        case Success(None)        => log.trace(s"$event saved")
        case _                    => throw new IllegalStateException("Impossibru")
      }

  private def hasOrder(id: Order.Id): Boolean = activeOrders.contains(id) || pendingCommands.contains(id) || orderDB.containsInfo(id)

  private def totalActiveOrders: Int = activeOrders.size + placementQueue.size

  private def getActiveLimitOrders(maybePair: Option[AssetPair]): Iterable[AcceptedOrder] =
    for {
      ao <- activeOrders.values
      if ao.isLimit && maybePair.forall(_ == ao.order.assetPair)
    } yield ao
}

object AddressActor {

  type Resp = api.MatcherResponse

  private val ExpirationThreshold = 50.millis

  private[dex] def activeStatus(ao: AcceptedOrder): OrderStatus =
    if (ao.amount == ao.order.amount) OrderStatus.Accepted else OrderStatus.PartiallyFilled(ao.order.amount - ao.amount, ao.order.matcherFee - ao.fee)

  /**
    * r = currentBalance |-| requiredBalance
    * @return None if ∀ (asset, v) ∈ r, v < 0
    *         else Some(r)
    */
  private def trySubtract(from: SpendableBalance, xs: SpendableBalance): Either[(Long, Asset), SpendableBalance] =
    xs.foldLeft[Either[(Long, Asset), SpendableBalance]](Right(from)) {
      case (r @ Left(_), _) => r
      case (curr, (_, 0))   => curr
      case (Right(curr), (assetId, amount)) =>
        val updatedAmount = curr.getOrElse(assetId, 0L) - amount
        Either.cond(updatedAmount >= 0, curr.updated(assetId, updatedAmount), (updatedAmount, assetId))
    }

  sealed trait Message

  sealed trait Query extends Message
  object Query {
    case class GetOrderStatus(orderId: ByteStr)                                              extends Query
    case class GetOrdersStatuses(assetPair: Option[AssetPair], orderListType: OrderListType) extends Query
    case object GetReservedBalance                                                           extends Query
    case class GetTradableBalance(forAssets: Set[Asset])                                     extends Query
  }

  sealed trait Reply
  object Reply {
    case class OrdersStatuses(xs: Seq[(ByteStr, OrderInfo[OrderStatus])]) extends Reply
    case class Balance(balance: Map[Asset, Long])                         extends Reply
  }

  sealed trait Command         extends Message
  sealed trait OneOrderCommand extends Command

  object Command {
    case class PlaceOrder(order: Order, isMarket: Boolean) extends OneOrderCommand {
      override lazy val toString =
        s"PlaceOrder(${if (isMarket) "market" else "limit"},id=${order
          .id()},s=${order.sender.toAddress},${order.assetPair},${order.orderType},p=${order.price},a=${order.amount})"

      def toAcceptedOrder(tradableBalance: Map[Asset, Long]): AcceptedOrder = if (isMarket) MarketOrder(order, tradableBalance) else LimitOrder(order)
    }

    case class CancelOrder(orderId: ByteStr)                             extends OneOrderCommand
    case class CancelOrders(orderIds: Set[ByteStr])                      extends Command
    case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command

    /**
      * @param newBalance Contains a new amount of changed assets
      */
    case class CancelNotEnoughCoinsOrders(newBalance: Map[Asset, Long]) extends Command
  }

  sealed trait Event {
    def orderId: Order.Id
  }

  sealed trait ValidationEvent extends Event

  object Event {
    case class ValidationFailed(orderId: Order.Id, error: MatcherError) extends ValidationEvent
    case class ValidationPassed(acceptedOrder: AcceptedOrder) extends ValidationEvent {
      override def orderId: Order.Id = acceptedOrder.id
    }
    case class StoreFailed(orderId: Order.Id, reason: MatcherError, queueEvent: QueueEvent) extends Event
  }

  sealed trait WsCommand extends Message
  object WsCommand {
    case class AddWsSubscription(client: typed.ActorRef[WsAddressState]) extends WsCommand
    private[AddressActor] case object PrepareDiffForWsSubscribers        extends WsCommand
  }

  private case class CancelExpiredOrder(orderId: ByteStr)
  private case class PendingCommand(command: OneOrderCommand, client: ActorRef)

  private case class InsufficientBalanceOrder(order: Order, insufficientAmount: Long, assetId: Asset)

  sealed abstract class OrderListType(val hasActive: Boolean, val hasClosed: Boolean) extends Product with Serializable
  object OrderListType {
    case object All        extends OrderListType(true, true)
    case object Empty      extends OrderListType(false, false)
    case object ActiveOnly extends OrderListType(true, false)
    case object ClosedOnly extends OrderListType(false, true)
  }

  final case class Settings(wsMessagesInterval: FiniteDuration, batchCancelTimeout: FiniteDuration, maxActiveOrders: Int)

  object Settings {
    val default: Settings = Settings(100.milliseconds, 20.seconds, 200)
  }
}
