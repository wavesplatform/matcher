package com.wavesplatform.dex.actors.address

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{typed, Actor, ActorRef, Cancellable, Props, Status}
import akka.pattern.{pipe, CircuitBreakerOpenException}
import akka.{actor => classic}
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.either._
import cats.syntax.group.catsSyntaxGroup
import com.wavesplatform.dex.actors.WorkingStash
import com.wavesplatform.dex.actors.address.AddressActor.Settings.default
import com.wavesplatform.dex.actors.address.AddressActor._
import com.wavesplatform.dex.api.http.entities.MatcherResponse
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsError, WsServerMessage}
import com.wavesplatform.dex.api.ws.state.WsAddressState
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.model.Denormalization.denormalizeAmountAndFee
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.{LoggerFacade, ScorexLogging}
import com.wavesplatform.dex.effect.Implicits.FutureOps
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError, UnexpectedError, WavesNodeConnectionBroken}
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCancelFailed, OrderCanceled, OrderCanceledReason, OrderExecuted, Event => OrderEvent}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.MatcherQueue.StoreValidatedCommand
import com.wavesplatform.dex.queue.ValidatedCommand
import com.wavesplatform.dex.time.Time
import org.slf4j.LoggerFactory

import java.time.{Instant, Duration => JDuration}
import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap, HashSet => MutableSet}
import scala.concurrent.duration._
import scala.concurrent.{Future, TimeoutException}
import scala.util.{Failure, Success, Try}

class AddressActor(
  owner: Address,
  time: Time,
  orderDB: OrderDB,
  validate: (AcceptedOrder, Map[Asset, Long]) => Future[Either[MatcherError, Unit]],
  store: StoreValidatedCommand,
  var recovered: Boolean,
  blockchain: BlockchainInteraction,
  settings: AddressActor.Settings = AddressActor.Settings.default
)(implicit efc: ErrorFormatterContext)
    extends Actor
    with WorkingStash
    with ScorexLogging {

  import context.dispatcher

  override protected lazy val log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))
  private val ignoreRef = context.system.toTyped.ignoreRef.toClassic

  private var placementQueue = Queue.empty[Order.Id]
  private val pendingCommands = MutableMap.empty[Order.Id, PendingCommand]

  private val activeOrders = MutableMap.empty[Order.Id, AcceptedOrder]
  private val expiration = MutableMap.empty[Order.Id, Cancellable]

  // Saves from cases when a client does multiple requests with the same order
  private val failedPlacements = MutableSet.empty[Order.Id]

  private var wsAddressState = WsAddressState.empty(owner)
  private var wsSendSchedule = Cancellable.alreadyCancelled

  private var balances = AddressBalance.empty

  // if (started) because we haven't pendingCommands during the start
  private val eventsProcessing: Receive = {
    case event: OrderAdded =>
      import event.order
      log.debug(s"OrderAdded(${order.id}, ${event.reason}, ${event.timestamp})")
      refreshOrderState(order, event)
      if (recovered) pendingCommands.remove(order.id).foreach { command =>
        log.trace(s"Confirming placement for ${order.id}")
        command.client ! Event.OrderAccepted(order.order)
      }

    case event: OrderExecuted =>
      log.debug(s"OrderExecuted(${event.submittedRemaining.id}, ${event.counterRemaining.id}), amount=${event.executedAmount}")
      List(event.submittedRemaining, event.counterRemaining).filter(_.order.sender.toAddress == owner).foreach(refreshOrderState(_, event))

    case event @ OrderCanceled(ao, reason, ts) =>
      val id = ao.id
      val isActive = activeOrders.contains(id)
      log.debug(s"OrderCanceled($id, $reason, $ts, isActive=$isActive)")
      if (isActive) refreshOrderState(ao, event)
      if (recovered) pendingCommands.remove(id).foreach { pc =>
        log.trace(s"Confirming cancellation for $id")
        pc.client ! Event.OrderCanceled(id)
      }

    case msg @ Command.ApplyBatch(events, balanceUpdates) =>
      log.info(s"Got $msg")
      // TODO DEX-1040 This code is not optimal, but it should work.
      // e.g. We need folding
      events.foreach(eventsProcessing(_))
      if (recovered && balanceUpdates.nonEmpty) working(Message.BalanceChanged(balanceUpdates)) // during start
      log.info("ApplyBatch applied")

    case command @ OrderCancelFailed(id, reason) =>
      if (recovered) pendingCommands.remove(id) match {
        case None => // Ok on secondary matcher
        case Some(pc) =>
          log.trace(s"Got $command, sending a response to a client")
          pc.client ! reason
      }

  }

  private val failuresProcessing: Receive = { case Status.Failure(e) =>
    log.error(s"Got $e", e)
  }

  private val recovering: Receive = eventsProcessing orElse failuresProcessing orElse {
    case AddressDirectoryActor.StartWork => context.become(starting)
    case x => stash(x)
  }

  private def starting: Receive = {
    recovered = true
    val spendingAssets = activeOrders.values.foldLeft(Set.empty[Asset]) { case (r, x) => r + x.spentAsset + x.feeAsset }
    blockchain
      .getPartialBalances(owner, spendingAssets)
      .safe
      .map(Command.SetInitialBalances)
      .pipeTo(self)

    {
      case command: Command.SetInitialBalances =>
        command.snapshot match {
          case Success(x) => balances = balances.withProbablyStale(x)
          case Failure(e) => throw new IllegalStateException("Can't receive initial balances, see logs", e)
        }
        activeOrders.values.foreach(x => scheduleExpiration(x.order))
        unstashAll()
        context.become(working)

      case x => stash(x)
    }: Receive
  }

  private val working: Receive = eventsProcessing orElse failuresProcessing orElse {
    case msg: GotPartialBalancesByRequest => balances = balances.withProbablyStale(msg.partial)
    case msg: GotFullBalancesByRequest =>
      msg.full match {
        case Left(e) =>
          wsAddressState.pendingSubscription.foreach(_.unsafeUpcast[WsServerMessage] ! WsError.from(e, time.correctedTime()))
          wsAddressState = wsAddressState.copy(pendingSubscription = Set.empty)

        case Right(x) =>
          balances = balances.withProbablyStale(x).withAllFetched
          sendWsSnapshotAndFlushPendingSubscriptions() // Received a snapshot for pending connections
      }

    case command: Command.PlaceOrder =>
      log.debug(s"Got $command")
      val orderId = command.order.id()

      if (totalActiveOrders >= settings.maxActiveOrders) sender() ! error.ActiveOrdersLimitReached(settings.maxActiveOrders)
      else if (failedPlacements.contains(orderId) || hasOrder(orderId)) sender() ! error.OrderDuplicate(orderId)
      else {
        val shouldProcess = placementQueue.isEmpty
        placementQueue = placementQueue.enqueue(orderId)
        pendingCommands.put(orderId, PendingCommand(command, sender()))
        if (shouldProcess) processNextPlacement()
        else log.trace(s"${placementQueue.headOption} is processing, moving $orderId to the queue")
      }

    case command: Command.CancelOrder =>
      import command.orderId
      log.debug(s"Got $command")
      pendingCommands.get(orderId) match {
        case Some(pc) =>
          sender() ! {
            pc.command match {
              case _: Command.PlaceOrder => error.OrderNotFound(orderId)
              case _: Command.CancelOrder => error.OrderCanceled(orderId)
            }
          }

        case None =>
          activeOrders.get(orderId) match {
            case None =>
              sender() ! {
                orderDB.status(orderId) match {
                  case OrderStatus.NotFound => error.OrderNotFound(orderId)
                  case _: OrderStatus.Cancelled => error.OrderCanceled(orderId)
                  case _: OrderStatus.Filled => error.OrderFull(orderId)
                }
              }

            case Some(ao) =>
              if (ao.isMarket) sender() ! error.MarketOrderCancel(orderId)
              else {
                pendingCommands.put(orderId, PendingCommand(command, sender()))
                cancel(ao.order, command.source)
              }
          }
      }

    case command: Command.CancelAllOrders =>
      val toCancelIds = getActiveLimitOrders(command.pair).map(_.id)
      if (toCancelIds.isEmpty) {
        log.debug(s"Got $command, nothing to cancel")
        sender() ! Event.BatchCancelCompleted(Map.empty)
      } else {
        log.debug(s"Got $command, to cancel: ${toCancelIds.mkString(", ")}")
        context.actorOf(BatchOrderCancelActor.props(toCancelIds.toSet, command.source, self, sender(), settings.batchCancelTimeout))
      }

    case command: Command.CancelOrders =>
      val allActiveOrderIds = getActiveLimitOrders(None).map(_.order.id()).toSet
      val toCancelIds = allActiveOrderIds.intersect(command.orderIds)
      val unknownIds = command.orderIds -- allActiveOrderIds

      log.debug(
        s"Got $command, total orders: ${allActiveOrderIds.size}, to cancel (${toCancelIds.size}): ${toCancelIds
          .mkString(", ")}, unknown ids (${unknownIds.size}): ${unknownIds.mkString(", ")}"
      )

      val initResponse = unknownIds.map(id => id -> error.OrderNotFound(id).asLeft[AddressActor.Event.OrderCanceled]).toMap
      if (toCancelIds.isEmpty) sender() ! Event.BatchCancelCompleted(initResponse)
      else
        context.actorOf(BatchOrderCancelActor.props(toCancelIds, command.source, self, sender(), settings.batchCancelTimeout, initResponse))

    case command @ CancelExpiredOrder(id) =>
      expiration.remove(id)
      val prefix = s"Got $command"
      activeOrders.get(id) match {
        case None => log.debug(s"$prefix for a not active order")
        case Some(ao) =>
          if ((ao.order.expiration - time.correctedTime()).max(0L).millis <= ExpirationThreshold) {
            log.debug(s"$prefix, storing cancel event")
            pendingCommands.put(
              id,
              PendingCommand(Command.CancelOrder(id, Command.Source.Expiration), ignoreRef)
            ) // To prevent orders being cancelled twice
            cancel(ao.order, Command.Source.Expiration)
          } else {
            log.trace(s"$prefix, can't find an active order")
            scheduleExpiration(ao.order)
          }
      }

    case msg: Message.BalanceChanged =>
      val changedAssets = msg.updates.changedAssets
      val spendableBefore = balances.tradableBalances(changedAssets)

      balances = balances.withFresh(msg.updates)
      val spendableAfter = balances.tradableBalances(changedAssets)

      if (wsAddressState.hasActiveSubscriptions) {
        wsAddressState = wsAddressState.putSpendableAssets(changedAssets)
        scheduleNextDiffSending()
      }

      val changesForAudit = spendableAfter.filter {
        // "exist" solves (I believe the rare case) when we receive: PlaceOrder, BalanceChanged, GotPartialBalancesByRequest
        case (asset, _) => spendableBefore.get(asset).exists(_ > 0)
      }

      val toCancel = getOrdersToCancel(changesForAudit).filterNot(ao => isCancelling(ao.order.id()))
      if (toCancel.isEmpty) log.trace(s"Got $msg, nothing to cancel")
      else {
        val cancelledText = toCancel.map(x => s"${x.insufficientAmount} ${x.assetId} for ${x.order.idStr()}").mkString(", ")
        log.debug(s"Got $msg, canceling ${toCancel.size} of ${activeOrders.size}: doesn't have $cancelledText")
        toCancel.foreach { x =>
          val id = x.order.id()
          pendingCommands.put(
            id,
            PendingCommand(Command.CancelOrder(id, Command.Source.BalanceTracking), ignoreRef)
          ) // To prevent orders being cancelled twice
          cancel(x.order, Command.Source.BalanceTracking)
        }
      }

    case Query.GetReservedBalance => sender() ! Reply.Balance(balances.openVolume.filter(_._2 > 0).asRight)

    case query: Query.GetTradableBalance =>
      val unknown = balances.needFetch(query.forAssets)
      if (unknown.isEmpty) sender() ! Reply.Balance(balances.tradableBalances(query.forAssets).asRight)
      else {
        val savedBalances = balances
        blockchain
          .getPartialBalances(owner, unknown)
          .transform {
            case Success(r) =>
              self ! GotPartialBalancesByRequest(r) // To store them locally
              Success(Reply.Balance(savedBalances.withProbablyStale(r).tradableBalances(query.forAssets).asRight))
            case Failure(e) =>
              log.error("Can't receive tradable balances, see logs", e)
              Success(Reply.Balance(error.WavesNodeConnectionBroken.asLeft))
          }
          .pipeTo(sender())
      }

    case Query.GetOrderStatus(orderId) =>
      sender() ! Reply.GetOrderStatus(activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(_.status))

    case Query.GetOrderStatusInfo(orderId) =>
      sender() ! Reply.OrdersStatusInfo(
        activeOrders
          .get(orderId)
          .map(ao => OrderInfo.v6(ao, ao.status)) orElse orderDB.getOrderInfo(orderId)
      )

    case Query.GetOrdersStatuses(maybePair, orderListType) =>
      val matchingActiveOrders =
        if (orderListType.hasActive)
          getActiveLimitOrders(maybePair)
            .map(ao => ao.id -> OrderInfo.v6(ao, ao.status))
            .toSeq
            .sorted
        else Seq.empty

      val matchingClosedOrders = if (orderListType.hasClosed) orderDB.getFinalizedOrders(owner, maybePair) else Seq.empty
      sender() ! Reply.OrdersStatuses(matchingActiveOrders ++ matchingClosedOrders)

    case Event.StoreFailed(orderId, reason, queueEvent) =>
      failedPlacements.add(orderId)
      pendingCommands.remove(orderId).foreach { command =>
        command.client ! reason
        queueEvent match {
          case ValidatedCommand.PlaceOrder(_) | ValidatedCommand.PlaceMarketOrder(_) =>
            activeOrders.remove(orderId).foreach { ao =>
              balances = balances.withoutVolume(ao.reservableBalance)
              if (wsAddressState.hasActiveSubscriptions) {
                wsAddressState = wsAddressState.putReservedAssets(ao.reservableBalance.keySet)
                scheduleNextDiffSending()
              }
            }
          case _ =>
        }
      }

    case _: Event.StoreSucceeded => if (failedPlacements.nonEmpty) failedPlacements.clear()

    case event: ValidationEvent =>
      log.trace(s"Got $event")
      placementQueue.dequeueOption.foreach { case (orderId, restQueue) =>
        if (orderId == event.orderId) {
          event match {
            case Event.ValidationPassed(ao) => pendingCommands.get(ao.id).foreach(_ => place(ao))
            case Event.ValidationFailed(_, reason) =>
              pendingCommands.remove(orderId).foreach { command =>
                log.trace(s"Confirming command for $orderId")
                command.client ! reason
              }
          }

          placementQueue = restQueue
          processNextPlacement()
        } else log.warn(s"Received stale $event for $orderId")
      }

    case WsCommand.AddWsSubscription(client) =>
      log.trace(s"[c=${client.path.name}] Added WebSocket subscription")
      wsAddressState = wsAddressState.addPendingSubscription(client)

      if (balances.allFetched) sendWsSnapshotAndFlushPendingSubscriptions()
      else {
        blockchain
          .getFullBalances(owner, balances.fetched)
          .transform {
            case Success(r) => Success(GotFullBalancesByRequest(r.asRight))
            case Failure(e) =>
              log.error("Can't receive tradable balances, see logs", e)
              Success(GotFullBalancesByRequest(error.WavesNodeConnectionBroken.asLeft))
          }
          .pipeTo(self)
        context.watch(client)
      }

    case WsCommand.RemoveWsSubscription(client) =>
      log.trace(s"[c=${client.path.name}] Removed WebSocket subscription")
      wsAddressState = wsAddressState.removeSubscription(client)
      context.unwatch(client)

    // It is time to send updates to clients. This block of code asks balances
    case WsCommand.SendDiff =>
      if (wsAddressState.hasActiveSubscriptions && wsAddressState.hasChanges) {
        wsAddressState = wsAddressState
          .sendDiffs(
            balances = mkWsBalances(balances.tradableBalances(wsAddressState.getAllChangedAssets)),
            orders = wsAddressState.getAllOrderChanges
          )
          .cleanOrderChanges()
          .cleanBalanceChanges()

        wsAddressState = wsAddressState.cleanOrderChanges()
      }
      wsSendSchedule = Cancellable.alreadyCancelled

    case classic.Terminated(wsSource) => wsAddressState = wsAddressState.removeSubscription(wsSource)
  }

  override val receive: Receive = if (recovered) starting else recovering // TODO retrieve balances

  /** Schedules next balances and order changes sending only if it wasn't scheduled before */
  private def scheduleNextDiffSending(): Unit =
    if (wsSendSchedule.isCancelled)
      wsSendSchedule = context.system.scheduler.scheduleOnce(settings.wsMessagesInterval, self, WsCommand.SendDiff)

  private def denormalizedBalanceValue(asset: Asset, decimals: Int)(balanceSource: Map[Asset, Long]): Double =
    denormalizeAmountAndFee(balanceSource.getOrElse(asset, 0L), decimals).toDouble

  private def mkWsBalances(tradableBalances: Map[Asset, Long]): Map[Asset, WsBalances] =
    tradableBalances
      .keys
      .flatMap { asset =>
        efc.assetDecimals(asset) match {
          case None =>
            log.error(s"Can't find asset decimals for $asset")
            List.empty // It is better to hide unknown assets rather than stop working
          case Some(decimals) =>
            val assetDenormalizedBalanceFrom = denormalizedBalanceValue(asset, decimals)(_)
            List(
              asset -> WsBalances(
                assetDenormalizedBalanceFrom(tradableBalances),
                assetDenormalizedBalanceFrom(balances.openVolume)
              )
            )
        }
      }
      .to(Map)

  private def isCancelling(id: Order.Id): Boolean = pendingCommands.get(id).exists(_.command.isInstanceOf[Command.CancelOrder])

  private def processNextPlacement(): Unit = placementQueue.dequeueOption.foreach { case (firstOrderId, _) =>
    pendingCommands.get(firstOrderId) match {
      case None =>
        throw new IllegalStateException(
          s"Can't find command for order $firstOrderId among pending commands: ${pendingCommands.keySet.mkString(", ")}"
        )
      case Some(nextCommand) =>
        nextCommand.command match {
          case command: Command.PlaceOrder =>
            val spendingAssets = Set(command.order.getSpendAssetId, command.order.feeAsset)
            val validationResult =
              for {
                tradableBalance <- {
                  val unknown = balances.needFetch(spendingAssets)
                  if (unknown.isEmpty) Future.successful(balances.tradableBalances(spendingAssets))
                  else {
                    val savedBalances = balances
                    blockchain
                      .getPartialBalances(owner, unknown)
                      .map { r =>
                        self ! GotPartialBalancesByRequest(r) // To store them locally
                        savedBalances.withProbablyStale(r).tradableBalances(spendingAssets)
                      }
                  }
                }
                ao = command.toAcceptedOrder(tradableBalance)
                r <- validate(ao, tradableBalance)
              } yield r match {
                case Left(error) => Event.ValidationFailed(command.order.id(), error)
                case Right(_) => Event.ValidationPassed(ao)
              }

            validationResult
              .recover {
                case ex: WavesNodeConnectionLostException =>
                  log.error("Waves Node connection lost", ex)
                  Event.ValidationFailed(command.order.id(), WavesNodeConnectionBroken)
                case ex =>
                  log.error("An unexpected error occurred", ex)
                  Event.ValidationFailed(command.order.id(), UnexpectedError)
              }
              .pipeTo(self)

          case x => throw new IllegalStateException(s"Can't process $x, only PlaceOrder is allowed")
        }
    }
  }

  private def scheduleExpiration(order: Order): Unit = if (!expiration.contains(order.id())) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def refreshOrderState(remaining: AcceptedOrder, event: OrderEvent): Unit = {

    val origActiveOrder = activeOrders.get(remaining.id)

    lazy val origReservableBalance = origActiveOrder.fold(Map.empty[Asset, Long])(_.reservableBalance)
    lazy val openVolumeDiff = remaining.reservableBalance |-| origReservableBalance

    val (status, unmatchable) = event match {
      case event: OrderCanceled =>
        val unmatchable = OrderCanceledReason.becameUnmatchable(event.reason)
        (OrderStatus.finalCancelStatus(remaining, event.reason), unmatchable)

      case _ => (remaining.status, false)
    }

    log.trace(s"New status of ${remaining.id} is $status")

    status match {
      case status: OrderStatus.Final =>
        expiration.remove(remaining.id).foreach(_.cancel())
        activeOrders.remove(remaining.id).foreach(ao => balances = balances.withoutVolume(ao.reservableBalance))
        orderDB.saveOrderInfo(remaining.id, owner, OrderInfo.v6(remaining, status))

      case _ =>
        activeOrders.put(remaining.id, remaining)
        balances = balances.withVolume(openVolumeDiff)
        status match {
          case OrderStatus.Accepted =>
            orderDB.saveOrder(remaining.order)
            scheduleExpiration(remaining.order)
          case _ =>
        }
    }

    if (wsAddressState.hasActiveSubscriptions) {
      wsAddressState = status match {
        case OrderStatus.Accepted => wsAddressState.putOrderUpdate(remaining.id, WsOrder.fromDomain(remaining, status))
        case _: OrderStatus.Cancelled => wsAddressState.putOrderStatusNameUpdate(remaining.id, status)
        case _ =>
          if (unmatchable) wsAddressState.putOrderStatusNameUpdate(remaining.id, status)
          else wsAddressState.putOrderFillingInfoAndStatusNameUpdate(remaining, status)
      }

      // OrderStatus.Accepted means that we've already notified clients about these balance changes after order passed validation (see def place)
      // Empty origActiveOrder means that:
      //  - for master DEX order was previously removed from active ones in handling of Event.StoreFailed
      //  - for slave DEX it is a new order and we have to send balance changes via WS API
      if (status != OrderStatus.Accepted || origActiveOrder.isEmpty)
        wsAddressState = wsAddressState.putReservedAssets(openVolumeDiff.keySet)

      scheduleNextDiffSending()
    }
  }

  /**
   * Use only if allAssetsKnown == true!
   */
  private def sendWsSnapshotAndFlushPendingSubscriptions(): Unit = {
    wsAddressState.sendSnapshot(
      // spendableBalances contains the latest balances
      balances = mkWsBalances(balances.allTradableBalances.filter(_._2 > 0)),
      orders = activeOrders.values.map(WsOrder.fromDomain(_)).to(Seq)
    )
    wsAddressState = wsAddressState.flushPendingSubscriptions()
  }

  private def getOrdersToCancel(actualBalance: Map[Asset, Long]): Queue[InsufficientBalanceOrder] = {
    val inProgress = pendingCancels
    // Now a user can have 100 active transaction maximum - easy to traverse.
    activeOrders.values.toVector
      .sortBy(_.order.timestamp)(Ordering[Long]) // Will cancel newest orders first
      .iterator
      .filter(x => x.isLimit && !inProgress.contains(x.id))
      .map(ao => (ao.order, ao.requiredBalance.view.filterKeys(actualBalance.contains).toMap))
      .foldLeft((actualBalance, Queue.empty[InsufficientBalanceOrder])) { case ((restBalance, toDelete), (order, requiredBalance)) =>
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

  private def pendingCancels: Set[Order.Id] =
    pendingCommands.collect { case (_, PendingCommand(Command.CancelOrder(id, _), _)) => id }.toSet

  private def cancellationInProgress(id: Order.Id): Boolean = pendingCommands.get(id).fold(false) {
    _.command match {
      case Command.CancelOrder(`id`, _) => true
      case _ => false
    }
  }

  private def place(ao: AcceptedOrder): Unit = {
    balances = balances.withVolume(ao.reservableBalance)
    activeOrders.put(ao.id, ao)

    if (wsAddressState.hasActiveSubscriptions) {
      wsAddressState = wsAddressState.putReservedAssets(ao.reservableBalance.keySet)
      scheduleNextDiffSending()
    }

    storeCommand(ao.id)(
      ao match {
        case ao: LimitOrder => ValidatedCommand.PlaceOrder(ao)
        case ao: MarketOrder => ValidatedCommand.PlaceMarketOrder(ao)
      }
    )
  }

  private def cancel(o: Order, source: Command.Source): Unit = storeCommand(o.id())(ValidatedCommand.CancelOrder(o.assetPair, o.id(), source))

  private def storeCommand(orderId: Order.Id)(command: ValidatedCommand): Unit =
    store(command)
      .transform {
        case Success(None) => Success(Some(error.FeatureDisabled))
        case Success(_) => Success(None)
        case Failure(e) =>
          val prefix = s"Store failed for $orderId, $command"
          log.warn(
            e match {
              case _: TimeoutException => s"$prefix: timeout during storing $command for $orderId"
              case _: CircuitBreakerOpenException => s"$prefix: fail fast due to circuit breaker"
              case _ => prefix
            },
            e
          )
          Success(Some(error.CanNotPersistEvent))
      }
      .onComplete {
        case Success(Some(error)) => self ! Event.StoreFailed(orderId, error, command)
        case Success(None) => self ! Event.StoreSucceeded(orderId, command); log.trace(s"$command saved")
        case _ => throw new IllegalStateException("Impossibru")
      }

  private def hasOrder(id: Order.Id): Boolean = activeOrders.contains(id) || pendingCommands.contains(id) || orderDB.containsInfo(id)

  private def totalActiveOrders: Int = activeOrders.size + placementQueue.size

  private def getActiveLimitOrders(maybePair: Option[AssetPair]): Iterable[AcceptedOrder] =
    for {
      ao <- activeOrders.values
      if ao.isLimit && maybePair.forall(_ == ao.order.assetPair)
    } yield ao

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(s"Failed on $message", reason)
    super.preRestart(reason, message)
  }

}

object AddressActor {

  trait BlockchainInteraction {
    def getFullBalances(address: Address, exclude: Set[Asset]): Future[AddressBalanceUpdates]
    def getPartialBalances(address: Address, assets: Set[Asset]): Future[AddressBalanceUpdates]
  }

  type Resp = MatcherResponse

  private val ExpirationThreshold = 50.millis

  def props(
    owner: Address,
    time: Time,
    orderDB: OrderDB,
    validate: (AcceptedOrder, Map[Asset, Long]) => Future[Either[MatcherError, Unit]],
    store: StoreValidatedCommand,
    started: Boolean,
    blockchain: BlockchainInteraction,
    settings: AddressActor.Settings = AddressActor.Settings.default
  )(implicit efc: ErrorFormatterContext): Props = Props(
    new AddressActor(
      owner,
      time,
      orderDB,
      validate,
      store,
      started,
      blockchain,
      settings
    )
  )

  /**
   * r = currentBalance |-| requiredBalance
   * @return None if ∀ (asset, v) ∈ r, v < 0
   *         else Some(r)
   */
  private def trySubtract(from: Map[Asset, Long], xs: Map[Asset, Long]): Either[(Long, Asset), Map[Asset, Long]] =
    xs.foldLeft[Either[(Long, Asset), Map[Asset, Long]]](Right(from)) {
      case (r @ Left(_), _) => r
      case (curr, (_, 0)) => curr
      case (Right(curr), (assetId, amount)) =>
        val updatedAmount = curr.getOrElse(assetId, 0L) - amount
        Either.cond(updatedAmount >= 0, curr.updated(assetId, updatedAmount), (updatedAmount, assetId))
    }

  sealed trait Message

  object Message {
    case class BalanceChanged(updates: AddressBalanceUpdates) extends Message
  }

  sealed trait Query extends Message

  object Query {
    case class GetOrderStatus(orderId: Order.Id) extends Query
    case class GetOrderStatusInfo(orderId: Order.Id) extends Query
    case class GetOrdersStatuses(assetPair: Option[AssetPair], orderListType: OrderListType) extends Query
    case object GetReservedBalance extends Query
    case class GetTradableBalance(forAssets: Set[Asset]) extends Query
  }

  sealed trait Reply

  object Reply {
    case class GetOrderStatus(x: OrderStatus) extends Reply
    case class OrdersStatuses(xs: Seq[(Order.Id, OrderInfo[OrderStatus])]) extends Reply
    case class Balance(balance: Either[MatcherError, Map[Asset, Long]]) extends Reply
    case class OrdersStatusInfo(maybeOrderStatusInfo: Option[OrderInfo[OrderStatus]]) extends Reply
  }

  sealed trait Command extends Message
  sealed trait OneOrderCommand extends Command

  object Command {

    case class SetInitialBalances(snapshot: Try[AddressBalanceUpdates]) extends Command

    case class ApplyBatch(events: Queue[Events.Event], balanceUpdates: AddressBalanceUpdates) extends Command

    case class PlaceOrder(order: Order, isMarket: Boolean) extends OneOrderCommand {

      override lazy val toString =
        s"PlaceOrder(${if (isMarket) "market" else "limit"},id=${order.id()},js=${order.jsonStr})"

      def toAcceptedOrder(tradableBalance: Map[Asset, Long]): AcceptedOrder =
        if (isMarket) MarketOrder(order, tradableBalance) else LimitOrder(order)

    }

    case class CancelOrder(orderId: Order.Id, source: Source) extends OneOrderCommand
    case class CancelOrders(orderIds: Set[Order.Id], source: Source) extends Command
    case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long, source: Source) extends Command

    sealed trait Source extends Product with Serializable

    object Source {
      // If you change this list, don't forget to change sourceToBytes
      case object NotTracked extends Source
      case object Request extends Source // User or admin, we don't know
      case object Expiration extends Source
      case object BalanceTracking extends Source
    }

  }

  sealed trait Event

  sealed trait ValidationEvent extends Event with Product with Serializable {
    def orderId: Order.Id
  }

  object Event {
    case class ValidationFailed(orderId: Order.Id, error: MatcherError) extends ValidationEvent

    case class ValidationPassed(acceptedOrder: AcceptedOrder) extends ValidationEvent {
      override def orderId: Order.Id = acceptedOrder.id
    }

    case class StoreFailed(orderId: Order.Id, reason: MatcherError, command: ValidatedCommand) extends Event
    case class StoreSucceeded(orderId: Order.Id, command: ValidatedCommand) extends Event
    // Now it doesn't matter whether an order executed or just added
    case class OrderAccepted(order: Order) extends Event
    case class OrderCanceled(orderId: Order.Id) extends Event
    case class BatchCancelCompleted(result: Map[Order.Id, Either[MatcherError, OrderCanceled]]) extends Event
  }

  sealed trait WsCommand extends Message

  object WsCommand {
    case class AddWsSubscription(client: typed.ActorRef[WsAddressChanges]) extends WsCommand
    case class RemoveWsSubscription(client: typed.ActorRef[WsAddressChanges]) extends WsCommand
    private[AddressActor] case object SendDiff extends WsCommand
  }

  private case class CancelExpiredOrder(orderId: Order.Id)
  private case class PendingCommand(command: OneOrderCommand, client: ActorRef)

  private case class InsufficientBalanceOrder(order: Order, insufficientAmount: Long, assetId: Asset)

  sealed abstract class OrderListType(val hasActive: Boolean, val hasClosed: Boolean) extends Product with Serializable

  object OrderListType {
    case object All extends OrderListType(true, true)
    case object Empty extends OrderListType(false, false)
    case object ActiveOnly extends OrderListType(true, false)
    case object ClosedOnly extends OrderListType(false, true)
  }

  final case class Settings(
    wsMessagesInterval: FiniteDuration = default.wsMessagesInterval,
    batchCancelTimeout: FiniteDuration = default.batchCancelTimeout,
    maxActiveOrders: Int = default.maxActiveOrders
  )

  object Settings {
    val default: Settings = Settings(100.milliseconds, 20.seconds, 200)
  }

  // TODO merge with BalanceUpdated and cancel orders too?
  final private case class GotPartialBalancesByRequest(partial: AddressBalanceUpdates)
  final private case class GotFullBalancesByRequest(full: Either[MatcherError, AddressBalanceUpdates])

}
