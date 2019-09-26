package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.pipe
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.NewAddressActor.OrderEvent
import com.wavesplatform.dex.api.NotImplemented
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.market.{BalanceActor, PlaceValidatorActor}
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent
import com.wavesplatform.dex.util.{WorkingStash, getSimpleName}
import com.wavesplatform.transaction.Asset
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
                   balanceActor: ActorRef,
                   storeActor: ActorRef,
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

  private val processingCommands = MutableMap.empty[Order.Id, Item]
  private val activeOrders       = MutableMap.empty[Order.Id, AcceptedOrder]
  private val expiration         = MutableMap.empty[ByteStr, Cancellable]

  private val placeValidatorActor = context.actorOf(Props(classOf[PlaceValidatorActor], self, balanceActor, orderBookCache), "validator")

  override def receive: Receive = {
    case command: PlaceOrder =>
      val orderId = command.order.id()
      if (processingCommands.contains(orderId)) sender ! api.OrderRejected(error.OrderDuplicate(orderId))
      else {
        processingCommands.put(orderId, Item(command, sender))
        placeValidatorActor ! command
      }

    case command: CancelOrder =>
      import command.orderId
      if (processingCommands.contains(orderId)) sender ! api.OrderCancelRejected(error.OrderNotFound(orderId))
      else
        activeOrders.get(orderId) match {
          case None =>
            val reason = orderDB.status(orderId) match {
              case OrderStatus.NotFound     => error.OrderNotFound(orderId)
              case _: OrderStatus.Cancelled => error.OrderCanceled(orderId)
              case _: OrderStatus.Filled    => error.OrderFull(orderId)
            }
            sender ! api.OrderCancelRejected(reason)

          case Some(ao) =>
            if (ao.isMarket) sender ! api.OrderCancelRejected(error.MarketOrderCancel(orderId))
            else {
              processingCommands.put(orderId, Item(command, sender))
              storeActor ! QueueEvent.Canceled(ao.order.assetPair, orderId)
            }
        }

    case command: CancelAllOrders        => // actor with sending all cancels?
    case command: CancelExpiredOrder     =>
    case AddressDirectory.StartSchedules =>
    case BalanceUpdated(actualBalance) =>
      getOrdersToCancel(actualBalance) |> { toCancel =>
        if (toCancel.nonEmpty) {
          log.debug(s"Canceling: $toCancel")
          toCancel.foreach(cancelledEvent => storeCanceled(cancelledEvent.assetPair, cancelledEvent.orderId))
        }
      }
    case event                           =>
  }

  private def applyEvent(mayBeEvent: Any): Unit = {
    mayBeEvent match {
      case OrderAdded(submitted, _) if submitted.order.sender.toAddress == owner =>
        log.trace(s"OrderAdded(${submitted.order.id()})")
        activeOrders.get(submitted.order.id()).foreach { acceptedOrder =>
          balanceActor ! BalanceActor.Command.Release(acceptedOrder.order.sender, acceptedOrder.reservableBalance)
        }
        handleOrderAdded(submitted)

      case e @ OrderExecuted(submitted, counter, _) =>
        log.trace(s"OrderExecuted(${submitted.order.id()}, ${counter.order.id()}), amount=${e.executedAmount}")
        handleOrderExecuted(e.submittedRemaining)
        handleOrderExecuted(e.counterRemaining)

      case OrderCanceled(ao, isSystemCancel, _) =>
        val id = ao.order.id()
        // submitted order gets canceled if it cannot be matched with the best counter order (e.g. due to rounding issues)
        processingCommands.remove(id).foreach { item => item.client ! api.OrderCanceled(id) }
        val isActive = activeOrders.contains(id)
        log.trace(s"OrderCanceled($id, system=$isSystemCancel, isActive=$isActive)")
        if (isActive) {
          activeOrders.get(ao.order.id()).foreach { acceptedOrder =>
            balanceActor ! BalanceActor.Command.Release(acceptedOrder.order.sender, acceptedOrder.reservableBalance)
          }
          handleOrderTerminated(ao, OrderStatus.finalStatus(ao, isSystemCancel))
        }
      case _ =>
    }

    processingCommands.get(event.orderId).foreach { item =>
      item.command match {
        case command: PlaceOrder  => applyEvent(command, event, item.client)
        case command: CancelOrder => applyEvent(command, event, item.client)
      }
    }
  }

  private def applyEvent(place: PlaceOrder, event: Any, client: ActorRef): Unit = event match {
    case PlaceValidatorActor.Event.ValidationFailed(id, reason) =>
      processingCommands.remove(id)
      client ! api.OrderRejected(reason)

    case PlaceValidatorActor.Event.ValidationPassed(id) =>
      balanceActor ! BalanceActor.Command.Reserve(place.order.sender, ???)
      storeActor ! StoreActor.Command.Save(QueueEvent.Placed(???))

    case OrderEvent.StoringFailed(id, reason) =>
      processingCommands.remove(id)
      client ! NotImplemented(reason) // error.FeatureDisabled

    case OrderEvent.StoringPassed(id) =>
    // processingCommands.remove(id)

    case OrderEvent.Accepted(id) =>
      processingCommands.remove(id)
      activeOrders.put(id, ???)
      client ! event

    case OrderEvent.Executed(id) =>
      processingCommands.remove(id)
      activeOrders.put(id, ???)
      client ! event

    case OrderEvent.Canceled(id) =>
      processingCommands.remove(id)
      balancesActor ! BalancesActor.Command.Release(place.order.sender, ???)
      client ! event // by matcher?

    case x =>
  }

  private def applyEvent(cancel: CancelOrder, event: Any, client: ActorRef): Unit = event match {
    case OrderEvent.StoringFailed(id, reason) =>
      processingCommands.remove(id)
      client ! NotImplemented(reason) // error.FeatureDisabled

    case OrderEvent.StoringPassed(id) =>
    // processingCommands.remove(id)

    case OrderEvent.Canceled(id) =>
      processingCommands.remove(id)
      activeOrders.remove(id).foreach { ao =>
        balancesActor ! BalancesActor.Command.Release(ao.order.sender, ???)
        client ! event
      }

    case x =>
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
    balanceActor ! BalanceActor.Command.Reserve(ao.order.sender, ao.reservableBalance)
    confirmPlacement(ao.order)
    scheduleExpiration(ao.order)
  }

  private def handleOrderExecuted(remaining: AcceptedOrder): Unit = if (remaining.order.sender.toAddress == owner) {
    balanceActor ! BalanceActor.Command.Release(remaining.order.sender, remaining.reservableBalance)
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

  sealed trait OrderType
  object OrderType {
    case object Limit  extends OrderType
    case object Market extends OrderType
  }

  case class PlaceOrder(order: Order, tpe: OrderType) extends Command {
    override lazy val toString = s"Place${getSimpleName(tpe)}Order(${getOrderInfo(order)})"
  }

  case class PlaceMarketOrder(order: Order) extends Command {
    override lazy val toString = s"PlaceMarketOrder(${getOrderInfo(order)})"
  }

  case class CancelOrder(orderId: ByteStr)                             extends Command
  case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command
  case class BalanceUpdated(actualBalance: Map[Asset, Long])           extends Command

  private case class CancelExpiredOrder(orderId: ByteStr)

  private case class Item(command: Command, client: ActorRef)
}
