package com.wavesplatform.dex.actors.address

import akka.actor.typed.scaladsl.adapter._
import akka.actor.{typed, Actor, ActorRef, Cancellable, Props, Stash, Status}
import akka.pattern.{pipe, CircuitBreakerOpenException}
import akka.{actor => classic}
import cats.data.NonEmptyList
import cats.instances.list._
import cats.instances.long._
import cats.syntax.foldable._
import cats.syntax.group.catsSyntaxGroup
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.actors.address.AddressActor.Command.{HasOrderBookEvents, ObservedTxData}
import com.wavesplatform.dex.actors.address.AddressActor.Settings.default
import com.wavesplatform.dex.actors.address.AddressActor._
import com.wavesplatform.dex.actors.address.BalancesFormatter.format
import com.wavesplatform.dex.api.http.entities.MatcherResponse
import com.wavesplatform.dex.api.ws.entities.{WsAddressFlag, WsAssetInfo, WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.WsAddressChanges
import com.wavesplatform.dex.api.ws.state.WsAddressState
import com.wavesplatform.dex.collections.{NegativeMap, PositiveMap}
import com.wavesplatform.dex.db.OrderDb
import com.wavesplatform.dex.db.OrderDb.orderInfoOrdering
import com.wavesplatform.dex.domain.account.{Address, PublicKey}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.model.Denormalization.denormalizeAmountAndFee
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionResult, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.LoggerFacade
import com.wavesplatform.dex.effect.Implicits.FutureOps
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.{ErrorFormatterContext, MatcherError, UnexpectedError, WavesNodeConnectionBroken}
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.grpc.integration.clients.domain.AddressBalanceUpdates
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException
import com.wavesplatform.dex.model.Events.{OrderAddedReason, OrderCancelFailed, OrderCanceledReason}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.MatcherQueue.StoreValidatedCommand
import com.wavesplatform.dex.queue.ValidatedCommand
import com.wavesplatform.dex.remote.Delay
import com.wavesplatform.dex.time.Time
import com.wavesplatform.dex.tool.KamonTraceUtils.runWithIgnoredSpan
import kamon.Kamon
import kamon.trace.Span
import org.slf4j.LoggerFactory

import java.time.{Instant, Duration => JDuration}
import java.util.concurrent.ConcurrentHashMap
import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap, HashSet => MutableSet}
import scala.concurrent.duration._
import scala.concurrent.{Future, TimeoutException}
import scala.util.{Failure, Success, Try}

class AddressActor(
  owner: Address,
  time: Time,
  orderDb: OrderDb[Future],
  validate: (AcceptedOrder, Map[Asset, Long]) => Future[Either[MatcherError, Unit]],
  store: StoreValidatedCommand,
  recovered: Boolean,
  blockchain: BlockchainInteraction,
  settings: AddressActor.Settings = AddressActor.Settings.default,
  getAssetDescription: Asset => BriefAssetDescription,
  log: LoggerFacade
) extends Actor
    with Stash {

  import context.dispatcher

  implicit val efc = ErrorFormatterContext.from(a => getAssetDescription(a).decimals)

  private val ignoreRef = context.system.toTyped.ignoreRef.toClassic

  private val enableRealtimeWs = settings.realtimeWsAddresses.contains(owner) || settings.wsMessagesInterval.length == 0

  private var isWorking = false

  // We need this because an order's validation is asynchronous and
  //  placing an order may affect following places.
  private var placementQueue = Queue.empty[EnqueuedOrder]
  private val pendingCommands = MutableMap.empty[Order.Id, PendingCommand]

  // orders that in process of adding, but may not be present in leveldb or in pendingCommands and activeOrders
  // See DEX-1435, DEX-1511
  private val processingOrders = ConcurrentHashMap.newKeySet[Order.Id]()

  private val activeOrders = MutableMap.empty[Order.Id, AcceptedOrder]
  private val expiration = MutableMap.empty[Order.Id, Cancellable]

  // Saves from cases when a client does multiple requests with the same order
  private val failedPlacements = MutableSet.empty[Order.Id]

  private var wsAddressState = WsAddressState.empty(owner)
  private var wsSendSchedule = Cancellable.alreadyCancelled

  private var balances = AddressBalance.empty

  // if (recovered) because we haven't pendingCommands during the start
  // DEX-1192 docs/places-and-cancels.md
  private val eventsProcessing: Receive = {
    case command: Command.ApplyOrderBookAdded =>
      import command.event
      import event.order

      // Empty means that:
      //  - for master DEX order was previously removed from active ones in handling of Event.StoreFailed
      //    but anyway saved to a queue and processed
      //  - for slave DEX it is a new order and we have to send balance changes via WS API
      val origActiveOrder = activeOrders.get(order.id)
      log.debug(
        s"OrderAdded(${order.id}, ${event.reason}, ${event.timestamp}), isNew=${origActiveOrder.isEmpty}, status: ${order.status}"
      )
      activeOrders.put(order.id, order)

      def reserve(xs: Map[Asset, Long]): Unit = {
        balances = balances.reserve(PositiveMap(xs))
        scheduleWs(wsAddressState.putChangedAssets(xs.keySet))
        log.info(s"[Balance] 0. 💵: ${format(balances.tradableBalance(xs.keySet).xs)}; ov Δ: ${format(xs)}")
      }

      lazy val orderReserve = order.reservableBalance
      order.status match {
        case OrderStatus.Accepted if event.reason == OrderAddedReason.RequestExecuted =>
          orderDb.saveOrder(order.order).onComplete {
            case Success(_) =>
            case Failure(th) =>
              //TODO probably inconsistent state can be introduced
              log.error("error while saving order", th)
          }

          origActiveOrder match {
            case Some(_) => // The order added before in the ValidationPassed -> place function, balances are already updated
            case None => reserve(orderReserve)
          }

        // Could happen only during a recovery
        case _ =>
          origActiveOrder match {
            case None => reserve(orderReserve) // Received a partially filled order
            case Some(origActiveOrder) => throw new IllegalStateException(s"Address already registered $origActiveOrder")
          }
      }

      scheduleExpiration(order.order)
      scheduleOrderWs(order, order.status, unmatchable = false, maybeMatchTx = None)

      if (isWorking)
        pendingCommands.remove(order.id).foreach { command =>
          log.trace(s"Confirming placement for ${order.id}")
          command.client ! Event.OrderAccepted(order.order)
        }

    case command: Command.ApplyOrderBookExecuted =>
      val orderExecutedUpdates = command.nonEmptyEvents.foldLeft(WsOrderExecutedUpdates()) {
        case (acc, event) =>
          val ownerRemainingOrders = List(event.event.counterRemaining, event.event.submittedRemaining).filter(_.order.sender.toAddress == owner)
          val txResult = event.expectedTx
          log.debug(
            s"OrderExecuted(${ownerRemainingOrders.map(o => s"${o.id} -> ${o.status}").mkString(
              ", "
            )}, tx=${txResult.transaction.id()}${txResult.error.fold("")(e => s", e=$e")}"
          )

          val cumulativeDiff = ownerRemainingOrders
            .foldMap { remaining =>
              remaining.status match {
                case status: OrderStatus.Final =>
                  expiration.remove(remaining.id).foreach(_.cancel())
                  orderDb.saveOrderInfo(remaining.id, owner, OrderInfo.v6(remaining, status)).onComplete { t =>
                    //TODO probably inconsistent state can be introduced
                    t.failed.foreach(log.error("error while saving order info", _))
                    processingOrders.remove(remaining.id)
                  }
                  activeOrders.remove(remaining.id) match {
                    case Some(origActiveOrder) => origActiveOrder.reservableBalance.inverse()
                    case None => throw new IllegalStateException(s"Can't find order ${remaining.id} during finalization")
                  }

                case _ =>
                  activeOrders.put(remaining.id, remaining) match {
                    case Some(origActiveOrder) => (remaining.reservableBalance |-| origActiveOrder.reservableBalance).filterNot(_._2 == 0)
                    case None => throw new IllegalStateException(s"Can't find order ${remaining.id}")
                  }
              }
            }
            .filterNot(_._2 == 0) // Fee could be 0 if an order executed by a small amount

          val (updated, changes) =
            balances.withExecuted(
              txResult.toOption.map(_.id()),
              AddressBalance.NotObservedTxData(ownerRemainingOrders.map(_.id), NegativeMap(cumulativeDiff))
            )
          balances = updated

          val reservedAssets = ownerRemainingOrders.flatMap(_.requiredBalance.keys).toSet
          val newReserved = balances.reserved.filter { case (asset, _) => reservedAssets.contains(asset) }
          log.info(
            s"[Balance] 1. 💵: ${format(balances.tradableBalance(cumulativeDiff.keySet).xs)}; e: ${format(cumulativeDiff)}, ov: ${format(newReserved)}"
          )
          val currentUpdate = WsOrderExecutedUpdates(ownerRemainingOrders.map(AcceptedOrderWithTx(_, txResult)), changes)
          acc merge currentUpdate
      }

      scheduleWs(
        wsAddressState
          .putChangedAssets(orderExecutedUpdates.changedAssets.changedAssets)
          .putTxsUpdate(
            orderExecutedUpdates.changedAssets.addedNotObservedTxs,
            orderExecutedUpdates.changedAssets.removedNotObservedTxs,
            orderExecutedUpdates.changedAssets.addedNotCreatedTxs,
            orderExecutedUpdates.changedAssets.removedNotCreatedTxs
          )
      )
      scheduleOrdersWs(orderExecutedUpdates.ownerRemainingOrders)

    case command: Command.ApplyOrderBookCanceled =>
      import command.event._
      val id = acceptedOrder.id

      val orderStatus = OrderStatus.finalCancelStatus(acceptedOrder, command.event.reason)
      val unmatchable = OrderCanceledReason.becameUnmatchable(command.event.reason)
      val origActiveOrder = activeOrders.remove(id)

      log.debug(
        s"OrderCanceled($id, $reason, $timestamp, isActive=${origActiveOrder.nonEmpty}), s: ${acceptedOrder.status} -> $orderStatus, un: $unmatchable"
      )

      origActiveOrder match {
        case None => // Can be received twice, because multiple matchers can cancel same order
        case Some(origActiveOrder) =>
          expiration.remove(id).foreach(_.cancel())
          orderDb.saveOrderInfo(id, owner, OrderInfo.v6(acceptedOrder, orderStatus)).onComplete { t =>
            //TODO probably inconsistent state can be introduced
            t.failed.foreach(log.error("error while saving order info", _))
            processingOrders.remove(id)
          }

          val orderReserve = origActiveOrder.reservableBalance

          balances = balances.cancelReservation(PositiveMap(orderReserve))
          scheduleWs(wsAddressState.putChangedAssets(orderReserve.keySet))
          log.info(s"[Balance] 2. 💵: ${format(balances.tradableBalance(orderReserve.keySet).xs)}; ov Δ: ${format(orderReserve)}")

          scheduleOrderWs(acceptedOrder, orderStatus, unmatchable, maybeMatchTx = None)
      }

      if (isWorking)
        pendingCommands.remove(id).foreach { pc =>
          log.trace(s"Confirming cancellation for $id")
          pc.client ! Event.OrderCanceled(id)
        }

    case command: Command.ChangeBalances => changeBalances(command.updates)

    case command: Command.ApplyBatch =>
      log.info("ApplyBatch")
      // Do not change an order of these operation, otherwise some orders can be canceled
      markTxsObserved(command.markTxsObserved.txsWithSpending)
      changeBalances(command.changedBalances.updates)
      log.info("ApplyBatch applied")

    case command: Command.MarkTxsObserved => markTxsObserved(command.txsWithSpending)

    case command @ OrderCancelFailed(id, reason, _) =>
      if (isWorking) {
        pendingCommands.remove(id) match {
          case None => // Ok on secondary matcher
          case Some(pc) =>
            log.trace(s"$command, sending a response to a client")
            pc.client ! reason
        }
        processingOrders.remove(id)
      }
  }

  private val failuresProcessing: Receive = { case Status.Failure(e) => log.error(s"$e", e) }

  private def starting(recovered: Boolean, gotBalances: Boolean): Receive = eventsProcessing orElse failuresProcessing orElse {
    case Command.CompleteRecovering => if (gotBalances) becomeWorking() else context.become(starting(recovered = true, gotBalances))

    case command: Command.SetInitialBalances =>
      command.snapshot match {
        case Failure(_) =>
          askFullBalances(command.attempt + 1)
          val message = s"Can't receive initial balances (${command.attempt})"
          if (command.attempt < 5) log.debug(message)
          else if (command.attempt < 10) log.warn(message)
          else log.error(message)

        case Success(x) =>
          balances = balances.withInit(x)
          log.info(s"[Balance] 3. 💵: ${format(balances.allTradableBalance.xs)}; i: ${format(x)}")
          if (recovered) becomeWorking() else context.become(starting(recovered, gotBalances = true))
      }

    case _ => stash()
  }

  private def becomeWorking(): Unit = {
    isWorking = true
    activeOrders.values.foreach(x => scheduleExpiration(x.order))
    unstashAll()

    context.become(working)
  }

  private def working: Receive = eventsProcessing orElse failuresProcessing orElse {
    // DEX-1192 docs/places-and-cancels.md
    case command: Command.PlaceOrder =>
      log.debug(s"$command")
      val orderId = command.order.id()
      if (totalActiveOrders >= settings.maxActiveOrders) sender() ! error.ActiveOrdersLimitReached(settings.maxActiveOrders)
      else if (failedPlacements.contains(orderId) || processingOrders.contains(orderId)) sender() ! error.OrderDuplicate(orderId)
      else {
        val origSender = sender()
        processingOrders.add(orderId)
        orderDb.containsInfo(orderId).onComplete {
          case Success(containsInfo) =>
            self.tell(Command.PlaceOrderFinalized(command, containsInfo), origSender)
          case Failure(th) =>
            log.error("error while retrieving order info", th)
            origSender ! UnexpectedError
        }
      }

    case command: Command.PlaceOrderFinalized =>
      log.debug(s"$command")
      val placeOrder = command.placeOrder
      val orderId = placeOrder.order.id()
      if (command.containsInfo || activeOrders.contains(orderId) || pendingCommands.contains(orderId))
        sender() ! error.OrderDuplicate(orderId)
      else {
        val shouldProcess = placementQueue.isEmpty
        placementQueue = placementQueue.enqueue(EnqueuedOrder(orderId, Kamon.currentSpan()))
        pendingCommands.put(orderId, PendingCommand(placeOrder, sender()))
        if (shouldProcess) processNextPlacement()
        else log.trace(s"${placementQueue.headOption} is processing, moving $orderId to the queue")
      }

    // DEX-1192 docs/places-and-cancels.md
    case command: Command.CancelOrder =>
      import command.orderId
      log.debug(s"$command")
      pendingCommands.get(orderId) match {
        case Some(pc) =>
          sender() ! {
            pc.command match {
              case _: Command.PlaceOrder =>
                error.OrderIsPlacing(orderId)
              case _: Command.CancelOrder =>
                error.OrderCanceled(orderId)
              case x =>
                log.error(s"found unexpected command '$x' while cancelling order")
                error.UnexpectedError
            }
          }

        case None =>
          activeOrders.get(orderId) match {
            case None =>
              orderDb.status(orderId).map {
                case OrderStatus.NotFound =>
                  error.OrderNotFound(orderId)
                case _: OrderStatus.Cancelled =>
                  error.OrderCanceled(orderId)
                case _: OrderStatus.Filled =>
                  error.OrderFull(orderId)
              }.recover { case th =>
                log.error("error while retrieving order status", th)
                UnexpectedError
              }.pipeTo(sender())

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
        log.debug(s"$command, nothing to cancel")
        sender() ! Event.BatchCancelCompleted(Map.empty)
      } else {
        log.debug(s"$command, to cancel: ${toCancelIds.mkString(", ")}")
        runWithIgnoredSpan {
          context.actorOf(BatchOrderCancelActor.props(
            toCancelIds.toList,
            command.source,
            self,
            sender(),
            settings.batchCancelTimeout
          ))
        }
      }

    case command: Command.CancelOrders =>
      log.debug("{}", command)
      runWithIgnoredSpan(context.actorOf(BatchOrderCancelActor.props(
        command.orderIds,
        command.source,
        self,
        sender(),
        settings.batchCancelTimeout
      )))

    case command @ CancelExpiredOrder(id) =>
      expiration.remove(id)
      val prefix = s"$command"
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

    case Query.GetCurrentState => sender() ! Reply.GetState(balances, placementQueue.map(_.orderId))

    case Query.GetReservedBalance => sender() ! Reply.GetBalance(balances.reserved.xs)

    case query: Query.GetTradableBalance => sender() ! Reply.GetBalance(balances.tradableBalance(query.forAssets).filter(_._2 > 0))

    case Query.GetOrderStatus(orderId) =>
      activeOrders.get(orderId) match {
        case Some(order) =>
          sender() ! Reply.GetOrderStatus(order.status)
        case None =>
          orderDb.status(orderId)
            .map(Reply.GetOrderStatus(_))
            .recover { case th =>
              log.error("error while retrieving order status", th)
              UnexpectedError
            }
            .pipeTo(sender())
      }

    case Query.GetOrderStatusInfo(orderId) =>
      activeOrders.get(orderId).map(ao => OrderInfo.v6(ao, ao.status)) match {
        case maybeOrderInfo @ Some(_) =>
          sender() ! Reply.GetOrdersStatusInfo(maybeOrderInfo)
        case None =>
          orderDb.getOrderInfo(orderId)
            .map(Reply.GetOrdersStatusInfo(_))
            .recover { case th =>
              log.error("error while retrieving order info", th)
              UnexpectedError
            }
            .pipeTo(sender())
      }

    case Query.GetOrdersStatuses(maybePair, orderListType) =>
      val matchingActiveOrders =
        if (orderListType.hasActive)
          getActiveLimitOrders(maybePair)
            .map(ao => ao.id -> OrderInfo.v6(ao, ao.status))
            .toList
            .sorted(orderInfoOrdering)
        else List.empty
      val matchingClosedOrders =
        if (orderListType.hasClosed)
          orderDb.getFinalizedOrders(owner, maybePair)
        else
          Future.successful(Seq.empty)
      matchingClosedOrders
        .map(x => Reply.GetOrderStatuses(matchingActiveOrders ++ x))
        .recover { case th =>
          log.error("error while retrieving finalized orders", th)
          UnexpectedError
        }.pipeTo(sender())

    case Event.StoreFailed(orderId, reason, queueEvent) =>
      failedPlacements.add(orderId)
      pendingCommands.remove(orderId).foreach { command =>
        command.client ! reason
        queueEvent match {
          case ValidatedCommand.PlaceOrder(_, _) | ValidatedCommand.PlaceMarketOrder(_, _) =>
            activeOrders.remove(orderId).foreach { ao =>
              val reservableBalance = ao.reservableBalance
              balances = balances.cancelReservation(PositiveMap(reservableBalance))
              log.info(
                s"[Balance] 4. 💵: ${format(balances.tradableBalance(reservableBalance.keySet).xs)}; ov -Δ: ${format(reservableBalance)}"
              )
              scheduleWs(wsAddressState.putChangedAssets(ao.reservableBalance.keySet))
            }
          case _ =>
        }
      }
      processingOrders.remove(orderId)

    case _: Event.StoreSucceeded => if (failedPlacements.nonEmpty) failedPlacements.clear()

    case event: ValidationEvent =>
      log.trace(s"$event")
      placementQueue.dequeueOption.foreach { case (EnqueuedOrder(orderId, _), restQueue) =>
        if (orderId == event.orderId) { // TODO Could this really happen?
          event match {
            case Event.ValidationPassed(ao) => pendingCommands.get(ao.id).foreach(_ => place(ao))
            case Event.ValidationFailed(_, reason) =>
              pendingCommands.remove(orderId).foreach { command =>
                log.trace(s"Confirming command for $orderId")
                command.client ! reason
              }
              processingOrders.remove(orderId)
          }

          placementQueue = restQueue
          processNextPlacement()
        } else log.warn(s"Received stale $event for $orderId")
      }

    case WsCommand.AddWsSubscription(client, flags, isDebug) =>
      log.trace(s"[c=${client.path.name}] Added WebSocket subscription (debug = $isDebug)")
      wsAddressState = wsAddressState.addSubscription(
        client,
        mkWsBalances(balances.allAssets, includeEmpty = false),
        activeOrders.values.map(WsOrder.fromDomain(_)).to(Seq),
        balances.notObservedTxs.view.mapValues(_.orderIds).toMap,
        balances.notCreatedTxs.view.mapValues(_.orderIds).toMap,
        flags,
        isDebug
      )
      context.watch(client)

    case WsCommand.RemoveWsSubscription(client) =>
      log.trace(s"[c=${client.path.name}] Removed WebSocket subscription")
      wsAddressState = wsAddressState.removeSubscription(client)
      context.unwatch(client)

    // It is time to send updates to clients. This block of code asks balances
    case WsCommand.SendDiff =>
      if (wsAddressState.hasActiveSubscriptions && wsAddressState.hasChanges) {
        val changedAssetInfo = mkWsBalances(wsAddressState.changedAssets, includeEmpty = true)
        wsAddressState = wsAddressState
          .sendDiffs(
            assetInfo = changedAssetInfo,
            orders = wsAddressState.getAllOrderChanges
          )
          .clean()
      }
      wsSendSchedule = Cancellable.alreadyCancelled

    case classic.Terminated(wsSource) => wsAddressState = wsAddressState.removeSubscription(wsSource)
  }

  override val receive: Receive = starting(recovered, gotBalances = false)
  askFullBalances(0)

  private def changeBalances(updates: AddressBalanceUpdates): Unit = {
    val changedAssets = updates.changedAssets
    val before = balances.balanceForAudit(changedAssets)

    balances = balances.withFresh(updates)
    scheduleWs(wsAddressState.putChangedAssets(changedAssets))

    if (isWorking) {
      val after = balances.balanceForAudit(changedAssets)
      val changesForAudit = after.collect { case (asset, v) if before.getOrElse(asset, 0L) > v => asset -> math.max(0, v) }
      val toCancel = getOrdersToCancel(changesForAudit).filterNot(x => isCancelling(x.order.id))
      if (toCancel.isEmpty)
        log.info(s"[Balance] 5. 💵: ${format(balances.tradableBalance(updates.changedAssets).xs)}; u: ${format(updates)}")
      else {
        log.info(
          s"[Balance] 6. 💵: ${format(balances.tradableBalance(updates.changedAssets).xs)}; u: ${format(updates)}; au: ${format(after)}"
        )
        val cancelledText =
          toCancel.map(x => s"${x.insufficientAmount} ${x.assetId} for ${x.order.id} (r: ${format(x.order.requiredBalance)})").mkString(", ")
        log.debug(s"Canceling ${toCancel.size}/${activeOrders.size}: doesn't have $cancelledText")
        toCancel.foreach { x =>
          val id = x.order.id
          pendingCommands.put(
            id,
            PendingCommand(Command.CancelOrder(id, Command.Source.BalanceTracking), ignoreRef)
          ) // To prevent orders being cancelled twice
          cancel(x.order.order, Command.Source.BalanceTracking)
        }
      }
    } else log.info(s"[Balance] 7. 💵: ${format(balances.tradableBalance(updates.changedAssets).xs)}; u: ${format(updates)}")
  }

  private def markTxsObserved(txs: Map[ExchangeTransaction.Id, ObservedTxData]): Unit = {
    log.info(
      s"Observed: ${txs.map { case (id, v) =>
        s"$id ${if (balances.notObservedTxs.contains(id)) "(wasn't before) " else ""}-> ${format(v.pessimisticChanges.xs)}"
      }.mkString(", ")}"
    )
    val (updated, changes) = txs.toList.foldl((balances, AddressBalance.Changes.empty)) {
      case ((r, prevChanges), (id, v)) =>
        val orderIds = v.orders.filter(_.sender.toAddress == owner).map(_.id())
        val notCreatedTxData = AddressBalance.NotCreatedTxData(orderIds, v.pessimisticChanges)
        val (b, changes) = r.withObserved(id, notCreatedTxData)
        (b, prevChanges |+| changes)
    }
    balances = updated
    if (changes.changedAssets.isEmpty) {
      log.info(s"[Balance] 8. au 💵: ${format(balances.balanceForAudit(txs.values.flatMap(_.pessimisticChanges.keySet).toSet))}")
      scheduleWs(
        wsAddressState
          .putTxsUpdate(
            changes.addedNotObservedTxs,
            changes.removedNotObservedTxs,
            changes.addedNotCreatedTxs,
            changes.removedNotCreatedTxs
          )
      )
    } else {
      log.info(s"[Balance] 9. otx 💵: ${format(balances.tradableBalance(changes.changedAssets).xs)}")
      scheduleWs(
        wsAddressState
          .putChangedAssets(changes.changedAssets)
          .putTxsUpdate(
            changes.addedNotObservedTxs,
            changes.removedNotObservedTxs,
            changes.addedNotCreatedTxs,
            changes.removedNotCreatedTxs
          )
      )
    }
  }

  /** Schedules next balances and order changes sending only if it wasn't scheduled before */
  private def scheduleNextDiffSending(): Unit =
    if (wsSendSchedule.isCancelled)
      if (enableRealtimeWs) self ! WsCommand.SendDiff
      else wsSendSchedule = context.system.scheduler.scheduleOnce(settings.wsMessagesInterval, self, WsCommand.SendDiff)

  private def mkWsBalances(forAssets: Set[Asset], includeEmpty: Boolean): Map[Asset, WsAssetInfo] = forAssets
    .flatMap { asset =>
      val assetDescription = getAssetDescription(asset)
      val tradable = balances.tradableBalance(asset)
      val reserved = balances.reserved.getOrElse(asset, 0L)
      if (includeEmpty || tradable > 0 || reserved > 0) {
        val wsBalances = WsBalances(
          tradable = denormalizeAmountAndFee(tradable, assetDescription.decimals).toDouble,
          reserved = denormalizeAmountAndFee(reserved, assetDescription.decimals).toDouble
        )
        List(
          asset -> WsAssetInfo(wsBalances, assetDescription.isNft)
        )
      } else Nil
    }.to(Map)

  private def isCancelling(id: Order.Id): Boolean = pendingCommands.get(id).exists(_.command.isInstanceOf[Command.CancelOrder])

  private def processNextPlacement(): Unit =
    placementQueue.dequeueOption.foreach { case (EnqueuedOrder(firstOrderId, parentSpan), _) =>
      val span = Kamon.spanBuilder("processNextPlacement").asChildOf(parentSpan).start()
      Kamon.runWithSpan(span) {
        pendingCommands.get(firstOrderId) match {
          case None =>
            throw new IllegalStateException(
              s"Can't find command for order $firstOrderId among pending commands: ${pendingCommands.keySet.mkString(", ")}"
            )
          case Some(nextCommand) =>
            nextCommand.command match {
              case command: Command.PlaceOrder =>
                val tradableBalances = balances.tradableBalance(Set(command.order.getSpendAssetId, command.order.feeAsset))
                val ao = command.toAcceptedOrder(tradableBalances.xs)
                validate(ao, tradableBalances.xs)
                  .map {
                    case Left(error) => Event.ValidationFailed(command.order.id(), error)
                    case Right(_) => Event.ValidationPassed(ao)
                  }
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
    }

  private def scheduleExpiration(order: Order): Unit = if (!expiration.contains(order.id())) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(
      s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}"
    )
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def scheduleOrderWs(
    remaining: AcceptedOrder,
    status: OrderStatus,
    unmatchable: Boolean,
    maybeMatchTx: Option[ExchangeTransaction]
  ): Unit = scheduleWs {
    status match {
      case OrderStatus.Accepted => wsAddressState.putOrderUpdate(remaining.id, WsOrder.fromDomain(remaining, status))
      case _: OrderStatus.Cancelled => wsAddressState.putOrderStatusNameUpdate(remaining.order, status)
      case _ =>
        // unmatchable can be only if OrderStatus.Filled
        if (unmatchable) wsAddressState.putOrderStatusNameUpdate(remaining.order, status)
        else wsAddressState.putOrderFillingInfoAndStatusNameUpdate(remaining, status, maybeMatchTx)
    }
  }

  private def scheduleOrdersWs(
    orders: List[AcceptedOrderWithTx]
  ): Unit = scheduleWs {
    orders.foldLeft(wsAddressState) { case (addressState, aoWtx) =>
      aoWtx.order.status match {
        case OrderStatus.Accepted => addressState.putOrderUpdate(aoWtx.order.id, WsOrder.fromDomain(aoWtx.order, aoWtx.order.status))
        case _: OrderStatus.Cancelled => addressState.putOrderStatusNameUpdate(aoWtx.order.order, aoWtx.order.status)
        case _ => addressState.putOrderFillingInfoAndStatusNameUpdate(aoWtx.order, aoWtx.order.status, aoWtx.tx.transaction.some)
      }
    }
  }

  private def getOrdersToCancel(actualNodeBalance: Map[Asset, Long]): Queue[InsufficientBalanceOrder] = {
    val inProgress = pendingCancels
    // Now a user can have 100 active transaction maximum - easy to traverse.
    activeOrders.values.toVector
      .sortBy(_.order.timestamp)(Ordering[Long]) // Will cancel newest orders first
      .iterator
      .filter(x => x.isLimit && !inProgress.contains(x.id))
      .map(ao => (ao, ao.requiredBalance.view.filterKeys(actualNodeBalance.contains).toMap))
      .foldLeft((actualNodeBalance, Queue.empty[InsufficientBalanceOrder])) { case ((restBalance, toDelete), (ao, requiredBalance)) =>
        trySubtract(restBalance, requiredBalance) match {
          case Right(updatedRestBalance) => (updatedRestBalance, toDelete)
          case Left((insufficientAmount, assetId)) =>
            val updatedToDelete =
              if (cancellationInProgress(ao.id)) toDelete
              else toDelete.enqueue(InsufficientBalanceOrder(ao, -insufficientAmount, assetId))
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
    activeOrders.put(ao.id, ao) // affects ApplyOrderBookAdded
    val reservableBalance = ao.reservableBalance

    balances = balances.reserve(PositiveMap(reservableBalance))
    log.info(
      s"[Balance] 10. o=${ao.id}; 💵: ${format(balances.tradableBalance(reservableBalance.keySet).xs)}; ov Δ: ${format(reservableBalance)}"
    )
    scheduleWs(wsAddressState.putChangedAssets(reservableBalance.keySet))

    storeCommand(ao.id)(
      ao match {
        case ao: LimitOrder => ValidatedCommand.PlaceOrder(ao)
        case ao: MarketOrder => ValidatedCommand.PlaceMarketOrder(ao)
      }
    )
  }

  private def cancel(o: Order, source: Command.Source): Unit =
    storeCommand(o.id())(ValidatedCommand.CancelOrder(o.assetPair, o.id(), source, Some(o.sender.toAddress)))

  // DEX-1192 docs/places-and-cancels.md
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

  private def totalActiveOrders: Int = activeOrders.size + placementQueue.size

  private def getActiveLimitOrders(maybePair: Option[AssetPair]): Iterable[AcceptedOrder] =
    for {
      ao <- activeOrders.values
      if ao.isLimit && maybePair.forall(_ == ao.order.assetPair)
    } yield ao

  private def scheduleWs(updatedF: => WsAddressState): Unit = if (wsAddressState.hasActiveSubscriptions) {
    wsAddressState = updatedF
    scheduleNextDiffSending()
  }

  private def askFullBalances(attempt: Int): Unit = {
    def send(): Unit = blockchain
      .getFullBalances(owner, Set.empty)
      .safe
      .map(Command.SetInitialBalances(_, attempt))
      .pipeTo(self)

    if (attempt == 0) send()
    else context.system.scheduler.scheduleOnce(Delay.fullJitter(100.millis, attempt, 5.seconds))(send())
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(s"Failed on $message", reason)
    super.preRestart(reason, message)
  }

}

object AddressActor {

  @FunctionalInterface
  trait BlockchainInteraction {
    def getFullBalances(address: Address, exclude: Set[Asset]): Future[AddressBalanceUpdates]
  }

  type Resp = MatcherResponse

  private val ExpirationThreshold = 50.millis
  private val logger = LoggerFacade(LoggerFactory.getLogger(AddressActor.getClass))

  def props(
    owner: Address,
    time: Time,
    orderDb: OrderDb[Future],
    validate: (AcceptedOrder, Map[Asset, Long]) => Future[Either[MatcherError, Unit]],
    store: StoreValidatedCommand,
    recovered: Boolean,
    blockchain: BlockchainInteraction,
    settings: AddressActor.Settings = AddressActor.Settings.default,
    getAssetDescription: Asset => BriefAssetDescription
  ): Props = Props(
    new AddressActor(
      owner,
      time,
      orderDb,
      validate,
      store,
      recovered,
      blockchain,
      settings,
      getAssetDescription,
      logger.copy(prefix = s"AddressActor[$owner]")
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

  private case class EnqueuedOrder(orderId: Order.Id, parentSpan: Span) {
    override def toString: String = orderId.toString
  }

  private case class AcceptedOrderWithTx(order: AcceptedOrder, tx: ExchangeTransactionResult[ExchangeTransactionV2])

  private case class WsOrderExecutedUpdates(
    ownerRemainingOrders: List[AcceptedOrderWithTx] = List.empty,
    changedAssets: AddressBalance.Changes = AddressBalance.Changes.empty
  ) {

    def merge(other: WsOrderExecutedUpdates): WsOrderExecutedUpdates =
      WsOrderExecutedUpdates(ownerRemainingOrders ++ other.ownerRemainingOrders, changedAssets |+| other.changedAssets)

  }

  case class OrderBookExecutedEvent(event: Events.OrderExecuted, expectedTx: ExchangeTransactionResult[ExchangeTransactionV2])
      extends HasOrderBookEvents {
    override def events: Seq[Events.Event] = List(event)
    override def affectedOrders: List[AcceptedOrder] = List(event.counter, event.submitted)
  }

  sealed trait Message

  sealed trait Query extends Message

  object Query {
    case class GetOrderStatus(orderId: Order.Id) extends Query
    case class GetOrdersStatuses(assetPair: Option[AssetPair], orderListType: OrderListType) extends Query
    case class GetOrderStatusInfo(orderId: Order.Id) extends Query
    case class GetTradableBalance(forAssets: Set[Asset]) extends Query
    case object GetReservedBalance extends Query
    case object GetCurrentState extends Query
  }

  sealed trait Reply

  object Reply {
    case class GetOrderStatus(x: OrderStatus) extends Reply
    case class GetOrderStatuses(xs: Seq[(Order.Id, OrderInfo[OrderStatus])]) extends Reply
    case class GetOrdersStatusInfo(maybeOrderStatusInfo: Option[OrderInfo[OrderStatus]]) extends Reply
    case class GetBalance(balance: Map[Asset, Long]) extends Reply
    case class GetState(balances: AddressBalance, placementQueue: Queue[Order.Id]) extends Reply
  }

  sealed trait Command extends Message
  sealed trait OneOrderCommand extends Command

  object Command {
    case object CompleteRecovering extends Command
    case class SetInitialBalances(snapshot: Try[AddressBalanceUpdates], attempt: Int) extends Command

    case class ApplyBatch(markTxsObserved: MarkTxsObserved, changedBalances: ChangeBalances) extends Command

    case class ChangeBalances(updates: AddressBalanceUpdates) extends Command

    case class ObservedTxData(orders: Seq[Order], pessimisticChanges: PositiveMap[Asset, Long])
    case class MarkTxsObserved(txsWithSpending: Map[ExchangeTransaction.Id, ObservedTxData]) extends Command

    sealed trait HasOrderBookEvents {
      def events: Iterable[Events.Event]
      def affectedOrders: List[AcceptedOrder]
    }

    case class ApplyOrderBookAdded(event: Events.OrderAdded) extends Command with HasOrderBookEvents {
      override def affectedOrders: List[AcceptedOrder] = List(event.order)

      override def events: Seq[Events.Event] = List(event)
    }

    case class ApplyOrderBookExecuted(nonEmptyEvents: NonEmptyList[OrderBookExecutedEvent]) extends Command with HasOrderBookEvents {
      override def affectedOrders: List[AcceptedOrder] = nonEmptyEvents.toList.flatMap(_.affectedOrders)

      override def events: Iterable[Events.Event] = nonEmptyEvents.toList.map(_.event)
    }

    object ApplyOrderBookExecuted {
      def apply(event: OrderBookExecutedEvent): ApplyOrderBookExecuted = ApplyOrderBookExecuted(nonEmptyEvents = NonEmptyList.one(event))
    }

    case class ApplyOrderBookCanceled(event: Events.OrderCanceled) extends Command with HasOrderBookEvents {
      override def affectedOrders: List[AcceptedOrder] = List(event.acceptedOrder)
      override def events: Seq[Events.Event] = List(event)
    }

    case class PlaceOrder(order: Order, isMarket: Boolean) extends OneOrderCommand {

      override lazy val toString =
        s"PlaceOrder(${if (isMarket) "market" else "limit"},id=${order.id()},js=${order.jsonStr})"

      def toAcceptedOrder(tradableBalance: Map[Asset, Long]): AcceptedOrder =
        if (isMarket) MarketOrder(order, tradableBalance) else LimitOrder(order)

    }

    case class PlaceOrderFinalized(placeOrder: PlaceOrder, containsInfo: Boolean) extends OneOrderCommand {

      import placeOrder._

      override lazy val toString =
        s"PlaceOrderFinalized(${if (isMarket) "market" else "limit"},id=${order.id()},js=${order.jsonStr},containsInfo=$containsInfo)"

    }

    case class CancelOrder(orderId: Order.Id, source: Source) extends OneOrderCommand
    case class CancelOrders(orderIds: List[Order.Id], source: Source) extends Command
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

    case class AddWsSubscription(client: typed.ActorRef[WsAddressChanges], flags: Set[WsAddressFlag] = Set.empty, isDebug: Boolean = false)
        extends WsCommand

    case class RemoveWsSubscription(client: typed.ActorRef[WsAddressChanges]) extends WsCommand
    private[AddressActor] case object SendDiff extends WsCommand
  }

  private case class CancelExpiredOrder(orderId: Order.Id)
  private case class PendingCommand(command: OneOrderCommand, client: ActorRef)

  private case class InsufficientBalanceOrder(order: AcceptedOrder, insufficientAmount: Long, assetId: Asset)

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
    maxActiveOrders: Int = default.maxActiveOrders,
    realtimeWsAccounts: Set[PublicKey] = default.realtimeWsAccounts
  ) {
    lazy val realtimeWsAddresses = realtimeWsAccounts.map(_.toAddress)
  }

  object Settings {
    val default: Settings = Settings(100.milliseconds, 20.seconds, 200, Set.empty[PublicKey])
  }

  sealed trait OrderRefreshResult extends Product with Serializable

  object OrderRefreshResult {
    case class Removed(volume: Map[Asset, Long]) extends OrderRefreshResult
    case class Updated(volumeDiff: Map[Asset, Long]) extends OrderRefreshResult
  }

  sealed trait AddressActorStatus extends Product with Serializable

  object AddressActorStatus {
    case class Starting(recovered: Boolean, gotBalances: Boolean) extends AddressActorStatus
    case object Working extends AddressActorStatus
  }

}
