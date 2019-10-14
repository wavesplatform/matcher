package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import cats.implicits._
import cats.kernel.Group
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.AddressActor._
import com.wavesplatform.dex.api.NotImplemented
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.market.{BalanceActor, MultipleOrderCancelActor, PlaceValidatorActor, StoreActor}
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.OrderValidator.MaxActiveOrders
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent
import com.wavesplatform.dex.util.{WorkingStash, getSimpleName}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import com.wavesplatform.dex.fp.MapImplicits.group
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group.catsSyntaxGroup
import org.slf4j.LoggerFactory

import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap}
import scala.concurrent.duration._

class AddressActor(owner: Address,
                   balanceActor: ActorRef,
                   storeActor: ActorRef,
                   time: Time,
                   orderDB: OrderDB,
                   hasOrderInBlockchain: Order.Id => Boolean,
                   orderBookCache: AssetPair => OrderBook.AggregatedSnapshot,
                   var enableSchedules: Boolean)
    extends Actor
    with WorkingStash
    with ScorexLogging {

  import context.dispatcher

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))

  private var requestCounter  = 0L
  private val pendingCommands = MutableMap.empty[Order.Id, PendingCommand]
  private val activeOrders    = MutableMap.empty[Order.Id, AcceptedOrder]
  private val expiration      = MutableMap.empty[ByteStr, Cancellable]

  private val placeValidatorActor = context.actorOf(Props(classOf[PlaceValidatorActor], self, balanceActor, orderBookCache), "validator")

  private def totalActiveOrders: Int = activeOrders.size + pendingCommands.values.count(_.command.isInstanceOf[Command.PlaceOrder])

  override def receive: Receive = {
    case command: Command.PlaceOrder =>
      log.trace(s"Got $command")
      val orderId = command.order.id()
      if (totalActiveOrders + 1 >= MaxActiveOrders) sender ! api.OrderRejected(error.ActiveOrdersLimitReached(MaxActiveOrders))
      else if (hasOrder(orderId)) sender ! api.OrderRejected(error.OrderDuplicate(orderId))
      else {
        requestCounter += 1
        pendingCommands.put(orderId, PendingCommand(requestCounter, command, command.order, sender))
        placeValidatorActor ! PlaceValidatorActor.Command.Validate(requestCounter, command)
      }

    case command: Command.CancelOrder =>
      import command.orderId
      log.trace(s"Got $command")
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
            requestCounter += 1
            pendingCommands.put(orderId, PendingCommand(requestCounter, command, ao.order, sender))
            storeCanceled(requestCounter, ao)
          }
      }

    case command @ Command.CancelAllOrders(maybePair, _) =>
      log.trace(s"Got $command")
      val orderIds = getActiveLimitOrders(maybePair).map(_.order.id())
      context.actorOf(MultipleOrderCancelActor.props(orderIds.toSet, self, sender))

    case Query.GetOrderStatus(orderId) => sender ! activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(activeStatus)

    case Query.GetOrdersStatuses(maybePair, onlyActive) =>
      val matchingActiveOrders = getActiveLimitOrders(maybePair)
        .map { ao =>
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
        }
        .toSeq
        .sorted

      log.trace(s"Collected ${matchingActiveOrders.length} active orders")
      val orders = if (onlyActive) matchingActiveOrders else orderDB.loadRemainingOrders(owner, maybePair, matchingActiveOrders)
      sender ! orders

    case event @ Event.BalanceUpdated(actualBalance) =>
      log.trace(s"Got $event")
      val toCancel = for {
        ao <- getOrdersToCancel(actualBalance)
        if pendingCommands.get(ao.order.id()).forall(!_.command.isInstanceOf[Command.CancelOrder])
      } yield ao

      if (toCancel.nonEmpty) {
        log.debug(s"Canceling (not enough balance): ${toCancel.map(_.order.id())}")
        toCancel.foreach { ao =>
          requestCounter += 1
          storeCanceled(requestCounter, ao)
        }
      }

    case event @ PlaceValidatorActor.Event.ValidationFailed(id, reason) =>
      log.trace(s"Got $event")
      pendingCommands.remove(id).foreach { item =>
        item.client ! api.OrderRejected(reason)
      }

    case event @ PlaceValidatorActor.Event.ValidationPassed(acceptedOrder) =>
      log.trace(s"Got $event")
      pendingCommands.get(acceptedOrder.order.id()).foreach { command =>
        reserve(acceptedOrder)
        storePlaced(command.requestId, acceptedOrder)
      }

    case event @ StoreActor.Event.StoreFailed(id, reason) =>
      log.trace(s"Got $event")
      pendingCommands.values.find(_.requestId == id).foreach { item =>
        item.client ! NotImplemented(reason) // error.FeatureDisabled
      }

    case CancelExpiredOrder(id) =>
      expiration.remove(id)
      activeOrders.get(id).foreach { ao =>
        if ((ao.order.expiration - time.correctedTime()).max(0L).millis <= ExpirationThreshold) {
          log.trace(s"Order $id expired, storing cancel event")
          requestCounter += 1
          storeCanceled(requestCounter, ao)
        } else scheduleExpiration(ao.order)
      }

    case AddressDirectory.StartSchedules =>
      if (!enableSchedules) {
        enableSchedules = true
        activeOrders.values.foreach(x => scheduleExpiration(x.order))
      }

    case OrderAdded(submitted, _) if submitted.order.sender.toAddress == owner =>
      import submitted.order
      log.trace(s"OrderAdded(${order.id()})")
      handleOrderAdded(submitted)
      pendingCommands.remove(order.id()).foreach { command =>
        log.trace(s"Confirming placement for ${order.id()}")
        command.client ! api.OrderAccepted(order)
      }

    case e @ OrderExecuted(submitted, counter, _) =>
//      if (!activeOrders.contains(submitted.order.id())) activeOrders.put(submitted.order.id(), submitted)
//      if (!activeOrders.contains(counter.order.id())) activeOrders.put(counter.order.id(), counter)
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

    case OrderCanceled(ao, isSystemCancel, _) =>
      val id = ao.order.id()
      // submitted order gets canceled if it cannot be matched with the best counter order (e.g. due to rounding issues)
      pendingCommands.remove(id).foreach { item =>
        item.client ! api.OrderCanceled(id)
      }
      val isActive = activeOrders.contains(id)
      log.trace(s"OrderCanceled($id, system=$isSystemCancel, isActive=$isActive)")
      if (isActive) {
        activeOrders.get(ao.order.id()).foreach(release)
        handleOrderTerminated(ao, OrderStatus.finalStatus(ao, isSystemCancel))
      }
  }

  private def scheduleExpiration(order: Order): Unit = if (enableSchedules) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def handleOrderAdded(ao: AcceptedOrder): Unit = {
    log.trace(s"Saving order ${ao.order.id()}, status is ${activeStatus(ao)}")
    orderDB.saveOrder(ao.order) // TODO do once when OrderAdded will be the first event
    val origAoReservableBalance = activeOrders.get(ao.order.id()).fold(Map.empty[Asset, Long])(_.reservableBalance)
    val diff = ao.reservableBalance |-| origAoReservableBalance
    balanceActor ! BalanceActor.Command.AppendReservedBalance(ao.order.sender, diff)

    activeOrders.put(ao.order.id(), ao)

//    release(ao)
//    reserve(ao)
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
    log.trace(s"Order ${ao.order.id()} terminated: $status")
    orderDB.saveOrder(ao.order)
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

  private def getOrdersToCancel(actualBalance: Map[Asset, Long]): Queue[AcceptedOrder] = {
    // Now a user can have 100 active transaction maximum - easy to traverse.
    activeOrders.values.toSeq
      .sortBy(_.order.timestamp)(Ordering[Long]) // Will cancel newest orders first
      .view
      .filter(_.isLimit)
      .map(ao => (ao, ao.requiredBalance filterKeys actualBalance.contains))
      .foldLeft { (actualBalance, Queue.empty[AcceptedOrder]) } {
        case ((currentBalance, ordersToCancel), (ao, requiredBalance)) =>
          subtract(currentBalance, requiredBalance) match {
            case Some(updatedCurrentBalances) => updatedCurrentBalances -> ordersToCancel
            case None                         => currentBalance         -> ordersToCancel.enqueue(ao)
          }
      }
      ._2
  }

  /**
    * r = currentBalance |-| requiredBalance
    * @return None if ∀ (asset, v) ∈ r, v < 0
    *         else Some(r)
    */
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

  private def storePlaced(requestId: Long, o: AcceptedOrder): Unit = {
    val event = o match {
      case o: LimitOrder  => QueueEvent.Placed(o)
      case o: MarketOrder => QueueEvent.PlacedMarket(o)
    }

    storeActor ! StoreActor.Command.Save(requestId, event)
  }

  private def storeCanceled(requestId: Long, o: AcceptedOrder): Unit =
    storeActor ! StoreActor.Command.Save(requestId, QueueEvent.Canceled(o.order.assetPair, o.order.id()))

  private def reserve(o: AcceptedOrder): Unit = if (!activeOrders.contains(o.order.id())) {
    log.trace(s"${o.order.id()} reservableBalance: ${o.reservableBalance}")
    balanceActor ! BalanceActor.Command.AppendReservedBalance(o.order.sender, o.reservableBalance)
    activeOrders.put(o.order.id(), o)
  }

  private def release(o: AcceptedOrder): Unit = if (activeOrders.contains(o.order.id())) {
    log.trace(s"${o.order.id()} reservableBalance: ${o.reservableBalance}")
    balanceActor ! BalanceActor.Command.AppendReservedBalance(o.order.sender, Group.inverse(o.reservableBalance))
    activeOrders.remove(o.order.id())
  }

  private def hasOrder(id: Order.Id): Boolean =
    pendingCommands.contains(id) || activeOrders.contains(id) || orderDB.containsInfo(id) || hasOrderInBlockchain(id)

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

  sealed trait Message

  sealed trait Query extends Message
  object Query {
    case class GetOrderStatus(orderId: ByteStr)                                     extends Query
    case class GetOrdersStatuses(assetPair: Option[AssetPair], onlyActive: Boolean) extends Query
  }

  sealed trait OrderType extends Message
  object OrderType {
    case object Limit  extends OrderType
    case object Market extends OrderType
  }

  sealed trait Command         extends Message
  sealed trait OneOrderCommand extends Command

  object Command {
    case class PlaceOrder(order: Order, tpe: OrderType) extends OneOrderCommand {
      override lazy val toString =
        s"PlaceOrder(tpe=${getSimpleName(tpe)},id=${order.id()},s=${order.sender.toAddress},${order.assetPair},${order.orderType},p=${order.price},a=${order.amount})"

      def toAcceptedOrder(tradableBalance: Map[Asset, Long]): AcceptedOrder = tpe match {
        case OrderType.Limit  => LimitOrder(order)
        case OrderType.Market => MarketOrder(order, tradableBalance)
      }
    }

    case class CancelOrder(orderId: ByteStr)                             extends OneOrderCommand
    case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command
  }

  // todo move to balances?
  sealed trait Event
  object Event {
    case class BalanceUpdated(actualBalance: Map[Asset, Long]) extends Event
  }

  private case class CancelExpiredOrder(orderId: ByteStr)

  private case class PendingCommand(requestId: Long, command: OneOrderCommand, order: Order, client: ActorRef)
}
