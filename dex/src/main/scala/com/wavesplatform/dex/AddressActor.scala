package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, Cancellable}
import akka.pattern.pipe
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.api.NotImplemented
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent
import com.wavesplatform.dex.util.WorkingStash
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair.assetIdStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import mouse.any._
import org.slf4j.LoggerFactory

import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class AddressActor(owner: Address,
                   spendableBalance: Asset => Future[Long],
                   cancelTimeout: FiniteDuration,
                   time: Time,
                   orderDB: OrderDB,
                   hasOrder: Order.Id => Boolean,
                   storeEvent: StoreEvent,
                   orderBookCache: AssetPair => OrderBook.AggregatedSnapshot,
                   var enableSchedules: Boolean)
    extends Actor
    with WorkingStash
    with ScorexLogging {

  import AddressActor._
  import context.dispatcher

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))

  private val pendingCancellation = MutableMap.empty[ByteStr, Promise[Resp]]
  private val pendingPlacement    = MutableMap.empty[ByteStr, Promise[Resp]]

  private val activeOrders = MutableMap.empty[Order.Id, AcceptedOrder]
  private val openVolume   = MutableMap.empty[Asset, Long].withDefaultValue(0L)
  private val expiration   = MutableMap.empty[ByteStr, Cancellable]

  private def reserve(acceptedOrder: AcceptedOrder): Unit = {
    activeOrders += acceptedOrder.order.id() -> acceptedOrder
    for { (assetId, b) <- acceptedOrder.reservableBalance if b != 0 } {
      val prevReserved    = openVolume(assetId)
      val updatedReserved = prevReserved + b
      log.trace(s"id=${acceptedOrder.order.id()}: $prevReserved + $b = $updatedReserved of ${assetIdStr(assetId)}")
      openVolume += assetId -> updatedReserved
    }
  }

  private def release(orderId: ByteStr): Unit = {
    for { acceptedOrder <- activeOrders.get(orderId); (assetId, b) <- acceptedOrder.reservableBalance if b != 0 } {
      val prevReserved    = openVolume(assetId)
      val updatedReserved = prevReserved - b
      log.trace(s"id=${acceptedOrder.order.id()}: $prevReserved - $b = $updatedReserved of ${assetIdStr(assetId)}")
      openVolume += assetId -> updatedReserved
    }
  }

  private def tradableBalance(assetId: Asset): Future[Long] = spendableBalance(assetId).map { _ - openVolume(assetId) }

  private def tradableBalancesByAssets(assets: Set[Asset]): Future[Map[Asset, Long]] = {
    assets.toList.traverse(asset => tradableBalance(asset).map(balance => asset -> balance)) map { _.toMap }
  }

  private def accountStateValidator(acceptedOrder: AcceptedOrder, tradableBalance: Asset => Long): OrderValidator.Result[AcceptedOrder] = {
    OrderValidator.accountStateAware(
      owner,
      tradableBalance,
      activeOrders.size,
      id => activeOrders.contains(id) || orderDB.containsInfo(id) || hasOrder(id),
      orderBookCache
    )(acceptedOrder)
  }

  private def placeOrder(order: Order, isMarket: Boolean): Unit = {
    pendingPlacement
      .get { order.id() }
      .fold {
        log.debug(s"New ${if (isMarket) "market order" else "order"}: ${order.json()}")

        tradableBalancesByAssets { Set(order.getSpendAssetId, order.matcherFeeAssetId) } flatMap { tradableBalance =>
          val acceptedOrder = if (isMarket) MarketOrder(order, tradableBalance) else LimitOrder(order)
          accountStateValidator(acceptedOrder, tradableBalance) match {
            case Left(error) => Future.successful { api.OrderRejected(error) }
            case Right(ao)   => reserve(ao); storePlaced(ao)
          }
        }
      }(_.future)
      .pipeTo(self)(sender)
  }

  private def handleCommands: Receive = {

    case PlaceLimitOrder(order)  => placeOrder(order, isMarket = false)
    case PlaceMarketOrder(order) => placeOrder(order, isMarket = true)

    case CancelOrder(id) =>
      pendingCancellation
        .get(id)
        .fold(
          activeOrders.get(id) match {
            case Some(ao) if ao.isLimit => storeCanceled(ao.order.assetPair, ao.order.id())
            case Some(_)                => Future.successful { api.OrderCancelRejected(error.MarketOrderCancel(id)) }
            case None =>
              val reason = orderDB.status(id) match {
                case OrderStatus.NotFound     => error.OrderNotFound(id)
                case _: OrderStatus.Cancelled => error.OrderCanceled(id)
                case _: OrderStatus.Filled    => error.OrderFull(id)
              }

              Future.successful { api.OrderCancelRejected(reason) }
          }
        )(_.future) pipeTo sender()

    case CancelAllOrders(maybePair, _) =>
      val batchCancelFutures =
        for { ao <- activeOrders.values if ao.isLimit && maybePair.forall(_ == ao.order.assetPair) } yield {
          val id = ao.order.id()
          val f  = pendingCancellation.get(id).fold(storeCanceled(ao.order.assetPair, id))(_.future)
          f.map(id -> _)
        }

      Future.sequence(batchCancelFutures).map(_.toMap).map(api.BatchCancelCompleted).pipeTo(sender())

    case CancelExpiredOrder(id) =>
      expiration.remove(id)
      for (lo <- activeOrders.get(id)) {
        if ((lo.order.expiration - time.correctedTime()).max(0L).millis <= ExpirationThreshold) {
          log.trace(s"Order $id expired, storing cancel event")
          storeCanceled(lo.order.assetPair, lo.order.id())
        } else scheduleExpiration(lo.order)
      }

    case AddressDirectory.StartSchedules =>
      if (!enableSchedules) {
        enableSchedules = true
        activeOrders.values.foreach(x => scheduleExpiration(x.order))
      }
  }

  private def store(id: ByteStr, event: QueueEvent, eventCache: MutableMap[ByteStr, Promise[Resp]], storeError: Resp): Future[Resp] = {
    val promisedResponse = Promise[Resp]
    eventCache += id -> promisedResponse
    storeEvent(event).transformWith {
      case Failure(e) =>
        log.error(s"Error persisting $event", e)
        Future.successful(storeError)
      case Success(r) =>
        r match {
          case None => Future.successful { NotImplemented(error.FeatureDisabled) }
          case Some(x) =>
            log.info(s"Stored $x")
            promisedResponse.future
        }
    }
  }

  private def storeCanceled(assetPair: AssetPair, id: ByteStr): Future[Resp] =
    store(id, QueueEvent.Canceled(assetPair, id), pendingCancellation, api.OrderCancelRejected(error.CanNotPersistEvent))

  private def storePlaced(acceptedOrder: AcceptedOrder): Future[Resp] =
    store(
      id = acceptedOrder.order.id(),
      event = acceptedOrder.fold[QueueEvent] { QueueEvent.Placed } { QueueEvent.PlacedMarket },
      eventCache = pendingPlacement,
      storeError = api.OrderRejected(error.CanNotPersistEvent)
    )

  private def confirmPlacement(order: Order): Unit = for (p <- pendingPlacement.remove(order.id())) {
    log.trace(s"Confirming placement for ${order.id()}")
    p.success(api.OrderAccepted(order))
  }

  private def handleStatusRequests: Receive = {
    case GetOrderStatus(orderId) => sender() ! activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(activeStatus)
    case GetOrdersStatuses(maybePair, onlyActive) =>
      log.trace(s"Loading ${if (onlyActive) "active" else "all"} ${maybePair.fold("")(_.toString + " ")}orders")

      val matchingActiveOrders = {
        for { ao <- activeOrders.values if ao.isLimit && maybePair.forall(_ == ao.order.assetPair) } yield
          ao.order.id() ->
            OrderInfo.v2(
              ao.order.orderType,
              ao.order.amount,
              ao.order.price,
              ao.order.matcherFee,
              ao.order.matcherFeeAssetId,
              ao.order.timestamp,
              activeStatus(ao),
              ao.order.assetPair
            )
      }.toSeq.sorted

      log.trace(s"Collected ${matchingActiveOrders.length} active orders")
      sender() ! (if (onlyActive) matchingActiveOrders else orderDB.loadRemainingOrders(owner, maybePair, matchingActiveOrders))

    case GetTradableBalance(pair) => tradableBalancesByAssets { Set(pair.amountAsset, pair.priceAsset) } pipeTo sender
    case GetReservedBalance       => sender() ! openVolume.filter { case (_, reserved) => reserved > 0 }.toMap
  }

  private def handleExecutionEvents: Receive = {
    case OrderAdded(submitted, _) if submitted.order.sender.toAddress == owner =>
      log.trace(s"OrderAdded(${submitted.order.id()})")
      release(submitted.order.id())
      handleOrderAdded(submitted)

    case e @ OrderExecuted(submitted, counter, _) =>
      log.trace(s"OrderExecuted(${submitted.order.id()}, ${counter.order.id()}), amount=${e.executedAmount}")
      handleOrderExecuted(e.submittedRemaining)
      handleOrderExecuted(e.counterRemaining)

    case OrderCanceled(ao, isSystemCancel, _) =>
      val id = ao.order.id()
      // submitted order gets canceled if it cannot be matched with the best counter order (e.g. due to rounding issues)
      confirmPlacement(ao.order)
      pendingCancellation.remove(id).foreach { _.success(api.OrderCanceled(id)) }
      val isActive = activeOrders.contains(id)
      log.trace(s"OrderCanceled($id, system=$isSystemCancel, isActive=$isActive)")
      if (isActive) {
        release(id)
        handleOrderTerminated(ao, OrderStatus.finalStatus(ao, isSystemCancel))
      }
  }

  private def handleBalanceChanges: Receive = {
    case BalanceUpdated(actualBalance) =>
      getOrdersToCancel(actualBalance) |> { toCancel =>
        if (toCancel.nonEmpty) {
          log.debug(s"Canceling: $toCancel")
          toCancel.foreach(cancelledEvent => storeCanceled(cancelledEvent.assetPair, cancelledEvent.orderId))
        }
      }
  }

  private def stashingOrderPlacement: Receive = {
    case response: Resp => sender ! response; context.become(activeState); unstashAll()
    case other          => stash(other)
  }

  private def scheduleExpiration(order: Order): Unit = if (enableSchedules) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def handleOrderAdded(ao: AcceptedOrder): Unit = {
    orderDB.saveOrder(ao.order)
    reserve(ao)
    confirmPlacement(ao.order)
    scheduleExpiration(ao.order)
  }

  private def handleOrderExecuted(remaining: AcceptedOrder): Unit = if (remaining.order.sender.toAddress == owner) {
    release(remaining.order.id())
    if (remaining.isValid) {
      handleOrderAdded(remaining)
    } else {
      confirmPlacement(remaining.order)
      val actualFilledAmount = remaining.order.amount - remaining.amount
      val actualFilledFee    = remaining.order.matcherFee - remaining.fee
      handleOrderTerminated(remaining, OrderStatus.Filled(actualFilledAmount, actualFilledFee))
    }
  }

  private def handleOrderTerminated(ao: AcceptedOrder, status: OrderStatus.Final): Unit = {
    log.trace(s"Order ${ao.order.id()} terminated: $status")
    orderDB.saveOrder(ao.order)
    pendingCancellation.remove(ao.order.id()).foreach(_.success(api.OrderCancelRejected(error.OrderFinalized(ao.order.id()))))
    expiration.remove(ao.order.id()).foreach(_.cancel())
    activeOrders.remove(ao.order.id())
    orderDB.saveOrderInfo(
      ao.order.id(),
      owner,
      OrderInfo.v2(
        ao.order.orderType,
        ao.order.amount,
        ao.order.price,
        ao.order.matcherFee,
        ao.order.matcherFeeAssetId,
        ao.order.timestamp,
        status,
        ao.order.assetPair
      )
    )
  }

  def activeState: Receive = handleCommands orElse sharedBehaviour

  def sharedBehaviour: Receive = handleExecutionEvents orElse handleStatusRequests orElse handleBalanceChanges

  def handleBalanceChanges: Receive = {
    case BalanceUpdated(actualBalance) =>
      getOrdersToCancel(actualBalance) |> { toCancel =>
        if (toCancel.nonEmpty) {
          log.debug(s"Canceling: $toCancel")
          toCancel.foreach(cancelledEvent => storeCanceled(cancelledEvent.assetPair, cancelledEvent.orderId))
        }
      }
  }

  def stashingState: Receive = sharedBehaviour orElse {
    case response: Resp => sender ! response; context.become(activeState); unstashAll()
    case other          => stash(other)
  }

  def receive: Receive = activeState

  private def getOrdersToCancel(actualBalance: Map[Asset, Long]): Queue[QueueEvent.Canceled] = {
    // Now a user can have 100 active transaction maximum - easy to traverse.
    activeOrders.values.toSeq
      .sortBy(_.order.timestamp)(Ordering[Long]) // Will cancel newest orders first
      .view
      .map(ao => (ao.order.id.value, ao.order.assetPair, ao.requiredBalance filterKeys actualBalance.contains))
      .foldLeft { (actualBalance, Queue.empty[QueueEvent.Canceled]) } {
        case ((currentBalance, ordersToCancel), (id, assetPair, requiredBalance)) =>
          subtract(currentBalance, requiredBalance) match {
            case Some(updatedCurrentBalances) => updatedCurrentBalances -> ordersToCancel
            case None =>
              currentBalance -> {
                if (pendingCancellation contains id) ordersToCancel else ordersToCancel enqueue QueueEvent.Canceled(assetPair, id)
              }
          }
      }
      ._2
  }

  private def subtract(currentBalance: Map[Asset, Long], requiredBalance: Map[Asset, Long]): Option[Map[Asset, Long]] = {
    requiredBalance.foldLeft[Option[Map[Asset, Long]]] { Some(currentBalance) } {
      case (None, _)                => None
      case (currentBalance, (_, 0)) => currentBalance
      case (Some(currentBalance), (assetId, requiredAmount)) =>
        val updatedAmount = currentBalance.getOrElse(assetId, 0L) - requiredAmount
        if (updatedAmount < 0) None
        else Some { currentBalance.updated(assetId, updatedAmount) }
    }
  }
}

object AddressActor {

  type Resp = api.MatcherResponse

  private val ExpirationThreshold = 50.millis

  private def activeStatus(ao: AcceptedOrder): OrderStatus =
    if (ao.amount == ao.order.amount) OrderStatus.Accepted else OrderStatus.PartiallyFilled(ao.order.amount - ao.amount, ao.order.matcherFee - ao.fee)

  private def getOrderInfo(order: Order): String = {
    s"${order.id()},${order.sender},${order.assetPair},${order.orderType},${order.price},${order.amount}"
  }

  sealed trait Command

  case class GetOrderStatus(orderId: ByteStr)                                     extends Command
  case class GetOrdersStatuses(assetPair: Option[AssetPair], onlyActive: Boolean) extends Command
  case class GetTradableBalance(assetPair: AssetPair)                             extends Command
  case object GetReservedBalance                                                  extends Command

  case class PlaceLimitOrder(order: Order) extends Command {
    override lazy val toString = s"PlaceLimitOrder(${getOrderInfo(order)})"
  }

  case class PlaceMarketOrder(order: Order) extends Command {
    override lazy val toString = s"PlaceMarketOrder(${getOrderInfo(order)})"
  }

  case class CancelOrder(orderId: ByteStr)                             extends Command
  case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command
  case class BalanceUpdated(actualBalance: Map[Asset, Long])           extends Command

  private case class CancelExpiredOrder(orderId: ByteStr)

}
