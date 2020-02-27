package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, ActorRef, Cancellable, Status}
import akka.pattern.pipe
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.functor.toFunctorOps
import cats.syntax.group.{catsSyntaxGroup, catsSyntaxSemigroup}
import com.wavesplatform.dex.AddressActor._
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.api.CanNotPersist
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError, UnexpectedError, WavesNodeConnectionBroken}
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalance
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.market.{BatchOrderCancelActor, CreateExchangeTransactionActor}
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
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
                   spendableBalance: Asset => Future[Long],
                   time: Time,
                   orderDB: OrderDB,
                   hasOrderInBlockchain: Order.Id => Future[Boolean],
                   store: StoreEvent,
                   orderBookCache: AssetPair => OrderBook.AggregatedSnapshot,
                   var enableSchedules: Boolean,
                   batchCancelTimeout: FiniteDuration = 20.seconds)(implicit efc: ErrorFormatterContext)
    extends Actor
    with ScorexLogging {

  import context.dispatcher

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))

  private var placementQueue  = Queue.empty[Order.Id]
  private val pendingCommands = MutableMap.empty[Order.Id, PendingCommand]

  private val activeOrders = MutableMap.empty[Order.Id, AcceptedOrder]
  private var openVolume   = Map.empty[Asset, Long]
  private val expiration   = MutableMap.empty[ByteStr, Cancellable]

  override def receive: Receive = {
    case command: Command.PlaceOrder =>
      import OrderValidator.MaxActiveOrders
      log.trace(s"Got $command")
      val orderId = command.order.id()

      if (pendingCommands.contains(orderId)) sender ! api.OrderRejected(error.OrderDuplicate(orderId))
      else if (totalActiveOrders >= MaxActiveOrders) sender ! api.OrderRejected(error.ActiveOrdersLimitReached(MaxActiveOrders))
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
      val toCancelIds = getActiveLimitOrders(command.pair).map(_.order.id())
      if (toCancelIds.isEmpty) {
        log.trace(s"Got $command, nothing to cancel")
        sender ! api.BatchCancelCompleted(Map.empty)
      } else {
        log.debug(s"Got $command, to cancel: ${toCancelIds.mkString(", ")}")
        context.actorOf(BatchOrderCancelActor.props(toCancelIds.toSet, self, sender, batchCancelTimeout))
      }

    case command: Command.CancelNotEnoughCoinsOrders =>
      val toCancel = getOrdersToCancel(command.newBalance).filterNot(ao => isCancelling(ao.order.id()))
      if (toCancel.isEmpty) log.trace(s"Got $command, nothing to cancel")
      else {
        val msg = toCancel
          .map(x => s"${x.insufficientAmount} ${x.assetId} for ${x.order.idStr()}")
          .mkString(", ")
        log.debug(s"Got $command, canceling ${toCancel.size} of ${activeOrders.size}: doesn't have $msg")
        toCancel.foreach(x => cancel(x.order))
      }

    case Query.GetReservedBalance            => sender ! Reply.Balance(openVolume)
    case Query.GetTradableBalance(forAssets) => getTradableBalance(forAssets).map(Reply.Balance).pipeTo(sender)

    case Query.GetOrderStatus(orderId) => sender ! activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(activeStatus)
    case Query.GetOrdersStatuses(maybePair, onlyActive) =>
      val matchingActiveOrders = getActiveLimitOrders(maybePair)
        .map { ao =>
          ao.order.id() -> OrderInfo.v3(ao, activeStatus(ao))
        }
        .toSeq
        .sorted

      log.trace(s"Collected ${matchingActiveOrders.length} active orders")
      val orders = if (onlyActive) matchingActiveOrders else orderDB.loadRemainingOrders(owner, maybePair, matchingActiveOrders)
      sender ! Reply.OrdersStatuses(orders)

    case event: Event.StoreFailed =>
      log.trace(s"Got $event")
      pendingCommands.remove(event.orderId).foreach { item =>
        item.client ! CanNotPersist(event.reason)
      }

    case event: ValidationEvent =>
      log.trace(s"Got $event")
      placementQueue.dequeueOption.foreach {
        case (orderId, restQueue) =>
          if (orderId == event.orderId) {
            event match {
              case Event.ValidationPassed(ao) => pendingCommands.get(ao.order.id()).foreach(_ => place(ao))
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

    case OrderAdded(submitted, _) if submitted.order.sender.toAddress == owner =>
      import submitted.order
      log.trace(s"OrderAdded(${order.id()})")
      handleOrderAdded(submitted)
      pendingCommands.remove(order.id()).foreach { command =>
        log.trace(s"Confirming placement for ${order.id()}")
        command.client ! api.OrderAccepted(order)
      }

    case e @ OrderExecuted(submitted, counter, _, _, _) =>
      log.trace(s"OrderExecuted(${submitted.order.id()}, ${counter.order.id()}), amount=${e.executedAmount}")
      handleOrderExecuted(e.submittedRemaining)
      handleOrderExecuted(e.counterRemaining)
      for {
        ao      <- List(submitted, counter)
        command <- pendingCommands.remove(ao.order.id())
      } {
        log.trace(s"Confirming placement for ${ao.order.id()}")
        command.client ! api.OrderAccepted(ao.order)
      }
      context.system.eventStream.publish(CreateExchangeTransactionActor.OrderExecutedObserved(owner, e))

    case OrderCanceled(ao, isSystemCancel, _) =>
      val id = ao.order.id()
      // submitted order gets canceled if it cannot be matched with the best counter order (e.g. due to rounding issues)
      pendingCommands.remove(id).foreach { pc =>
        pc.command match {
          case command: Command.PlaceOrder =>
            log.trace(s"Confirming placement for $id")
            pc.client ! api.OrderAccepted(command.order) // TODO remove after OrderBook refactoring
          case _: Command.CancelOrder =>
            log.trace(s"Confirming cancelation for $id")
            pc.client ! api.OrderCanceled(id)
        }
      }
      val isActive = activeOrders.contains(id)
      log.trace(s"OrderCanceled($id, system=$isSystemCancel, isActive=$isActive)")
      if (isActive) handleOrderTerminated(ao, OrderStatus.finalStatus(ao, isSystemCancel))

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
              val validationResult = {
                for {
                  hasOrderInBlockchain <- hasOrderInBlockchain { command.order.id() }
                  tradableBalance      <- getTradableBalance(Set(command.order.getSpendAssetId, command.order.feeAsset))
                } yield {
                  val ao = command.toAcceptedOrder(tradableBalance)
                  accountStateValidator(ao, tradableBalance, hasOrderInBlockchain) match {
                    case Left(error) => Event.ValidationFailed(ao.order.id(), error)
                    case Right(_)    => Event.ValidationPassed(ao)
                  }
                }
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

  private def accountStateValidator(acceptedOrder: AcceptedOrder,
                                    tradableBalance: Map[Asset, Long],
                                    hasOrderInBlockchain: Boolean): OrderValidator.Result[AcceptedOrder] = {
    OrderValidator
      .accountStateAware(acceptedOrder.order.sender,
                         tradableBalance.withDefaultValue(0L),
                         totalActiveOrders,
                         hasOrder(_, hasOrderInBlockchain),
                         orderBookCache)(acceptedOrder)
  }

  private def getTradableBalance(forAssets: Set[Asset]): Future[Map[Asset, Long]] = {
    Future
      .traverse(forAssets)(asset => spendableBalance(asset) tupleLeft asset)
      .map(xs => (xs.toMap |-| openVolume).withDefaultValue(0L))
  }

  private def scheduleExpiration(order: Order): Unit = if (enableSchedules && !expiration.contains(order.id())) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def handleOrderAdded(ao: AcceptedOrder): Unit = {
    log.trace(s"Saving order ${ao.order.id()}, new status is ${activeStatus(ao)}")
    orderDB.saveOrder(ao.order) // TODO do once when OrderAdded will be the first event. UP: it happens everytime orderbooks are restored, FIX this
    val origAoReservableBalance = activeOrders.get(ao.order.id()).fold(Map.empty[Asset, Long])(_.reservableBalance)
    openVolume = openVolume |+| (ao.reservableBalance |-| origAoReservableBalance)

    activeOrders.put(ao.order.id(), ao)
    scheduleExpiration(ao.order)
  }

  private def handleOrderExecuted(remaining: AcceptedOrder): Unit = if (remaining.order.sender.toAddress == owner) {
    if (remaining.isValid) handleOrderAdded(remaining)
    else {
      val actualFilledAmount = remaining.order.amount - remaining.amount
      val actualFilledFee    = remaining.order.matcherFee - remaining.fee
      handleOrderTerminated(remaining, OrderStatus.Filled(actualFilledAmount, actualFilledFee))
    }
  }

  private def handleOrderTerminated(ao: AcceptedOrder, status: OrderStatus.Final): Unit = {
    log.trace(s"Order ${ao.order.id()} terminated, new status is $status")
    orderDB.saveOrder(ao.order)
    expiration.remove(ao.order.id()).foreach(_.cancel())
    activeOrders.remove(ao.order.id()).foreach { ao =>
      openVolume = openVolume |-| ao.reservableBalance
    }
    orderDB.saveOrderInfo(ao.order.id(), owner, OrderInfo.v3(ao, status))
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
    activeOrders.put(ao.order.id(), ao)
    storeEvent(ao.order.id())(
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
        case Success(Some(error)) => self ! Event.StoreFailed(orderId, error)
        case Success(None)        => log.trace(s"$event saved")
        case _                    => throw new IllegalStateException("Impossibru")
      }

  private def hasOrder(id: Order.Id, hasOrderInBlockchain: Boolean): Boolean =
    activeOrders.contains(id) || orderDB.containsInfo(id) || hasOrderInBlockchain

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

  private def activeStatus(ao: AcceptedOrder): OrderStatus =
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
    case class GetOrderStatus(orderId: ByteStr)                                     extends Query
    case class GetOrdersStatuses(assetPair: Option[AssetPair], onlyActive: Boolean) extends Query
    case object GetReservedBalance                                                  extends Query
    case class GetTradableBalance(forAssets: Set[Asset])                            extends Query
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
      override def orderId: Order.Id = acceptedOrder.order.id()
    }
    case class StoreFailed(orderId: Order.Id, reason: MatcherError) extends Event
  }

  private case class CancelExpiredOrder(orderId: ByteStr)
  private case class PendingCommand(command: OneOrderCommand, client: ActorRef)

  private case class InsufficientBalanceOrder(order: Order, insufficientAmount: Long, assetId: Asset)
}
