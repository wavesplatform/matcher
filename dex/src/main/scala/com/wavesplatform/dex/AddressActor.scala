package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, ActorRef, Cancellable}
import akka.pattern.pipe
import cats.instances.future.catsStdInstancesForFuture
import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.functor.toFunctorOps
import cats.syntax.group.{catsSyntaxGroup, catsSyntaxSemigroup}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.AddressActor._
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.api.NotImplemented
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.market.MultipleOrderCancelActor
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.OrderValidator.MaxActiveOrders
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent
import com.wavesplatform.dex.util.WorkingStash
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import org.slf4j.LoggerFactory

import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap}
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success

class AddressActor(owner: Address,
                   spendableBalance: Asset => Future[Long],
                   time: Time,
                   orderDB: OrderDB,
                   hasOrderInBlockchain: Order.Id => Boolean,
                   store: StoreEvent,
                   orderBookCache: AssetPair => OrderBook.AggregatedSnapshot,
                   var enableSchedules: Boolean)
    extends Actor
    with WorkingStash
    with ScorexLogging {

  import context.dispatcher

  protected override lazy val log = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))

  private var placementQueue  = Queue.empty[Order.Id]
  private val pendingCommands = MutableMap.empty[Order.Id, PendingCommand]

  private val activeOrders = MutableMap.empty[Order.Id, AcceptedOrder]
  private val expiration   = MutableMap.empty[ByteStr, Cancellable]
  private var openVolume   = Map.empty[Asset, Long]

  override def receive: Receive = {
    case command: Command.PlaceOrder =>
      log.trace(s"Got $command")
      val orderId = command.order.id()
      if (totalActiveOrders + 1 >= MaxActiveOrders) sender ! api.OrderRejected(error.ActiveOrdersLimitReached(MaxActiveOrders))
      else if (hasOrder(orderId)) sender ! api.OrderRejected(error.OrderDuplicate(orderId))
      else {
        val shouldProcessNext = placementQueue.isEmpty
        placementQueue = placementQueue.enqueue(orderId)
        pendingCommands.put(orderId, PendingCommand(command, sender))
        if (shouldProcessNext) processFirstPlacement()
      }

    case command: Command.CancelOrder =>
      import command.orderId
      log.trace(s"Got $command")
      activeOrders.get(orderId) match {
        case None =>
          sender ! api.OrderCancelRejected(orderDB.status(orderId) match {
            case OrderStatus.NotFound     => error.OrderNotFound(orderId)
            case _: OrderStatus.Cancelled => error.OrderCanceled(orderId)
            case _: OrderStatus.Filled    => error.OrderFull(orderId)
          })

        case Some(ao) =>
          if (ao.isMarket) sender ! api.OrderCancelRejected(error.MarketOrderCancel(orderId))
          else {
            pendingCommands.put(orderId, PendingCommand(command, sender))
            cancel(ao)
          }
      }

    case command: Command.CancelAllOrders =>
      log.trace(s"Got $command")
      val orderIds = getActiveLimitOrders(command.pair).map(_.order.id())
      context.actorOf(MultipleOrderCancelActor.props(orderIds.toSet, self, sender))

    case command: Command.CancelNotEnoughCoinsOrders =>
      log.trace(s"Got $command")
      val toCancel = getOrdersToCancel(command.newBalance).filterNot(ao => isCancelling(ao.order.id()))
      if (toCancel.nonEmpty) {
        log.debug(s"Canceling (not enough balance): ${toCancel.map(_.order.id())}")
        toCancel.foreach(cancel)
      }

    case Query.GetReservedBalance            => sender ! Reply.Balance(openVolume)
    case Query.GetTradableBalance(forAssets) => getTradableBalance(forAssets).map(Reply.Balance).pipeTo(sender)

    case Query.GetOrderStatus(orderId) => sender ! activeOrders.get(orderId).fold[OrderStatus](orderDB.status(orderId))(activeStatus)
    case Query.GetOrdersStatuses(maybePair, onlyActive) =>
      val matchingActiveOrders = getActiveLimitOrders(maybePair)
        .map { ao =>
          ao.order.id() -> OrderInfo.v2(ao.order, activeStatus(ao))
        }
        .toSeq
        .sorted

      log.trace(s"Collected ${matchingActiveOrders.length} active orders")
      val orders = if (onlyActive) matchingActiveOrders else orderDB.loadRemainingOrders(owner, maybePair, matchingActiveOrders)
      sender ! Reply.OrdersStatuses(orders)

    case event: Event.StoreFailed =>
      log.trace(s"Got $event")
      pendingCommands.get(event.orderId).foreach { item =>
        item.client ! NotImplemented(event.reason) // error.FeatureDisabled
      }

    case event: ValidationEvent =>
      log.trace(s"Got $event")
      placementQueue.dequeueOption.foreach {
        case (orderId, restQueue) =>
          if (orderId == event.orderId) {
            event match {
              case Event.ValidationPassed(ao)         => pendingCommands.get(ao.order.id()).foreach(_ => place(ao))
              case Event.ValidationFailed(id, reason) => pendingCommands.remove(id).foreach(_.client ! api.OrderRejected(reason))
            }

            placementQueue = restQueue
            processFirstPlacement()
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

    case e @ OrderExecuted(submitted, counter, _) =>
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
      pendingCommands.remove(id).foreach(_.client ! api.OrderCanceled(id))
      val isActive = activeOrders.contains(id)
      log.trace(s"OrderCanceled($id, system=$isSystemCancel, isActive=$isActive)")
      if (isActive) handleOrderTerminated(ao, OrderStatus.finalStatus(ao, isSystemCancel))

    case CancelExpiredOrder(id) =>
      expiration.remove(id)
      activeOrders.get(id).foreach { ao =>
        if ((ao.order.expiration - time.correctedTime()).max(0L).millis <= ExpirationThreshold) {
          log.trace(s"Order $id expired, storing cancel event")
          cancel(ao)
        } else scheduleExpiration(ao.order)
      }

    case AddressDirectory.StartSchedules =>
      if (!enableSchedules) {
        enableSchedules = true
        activeOrders.values.foreach(x => scheduleExpiration(x.order))
      }
  }

  private def isCancelling(id: Order.Id): Boolean = pendingCommands.get(id).exists(_.command.isInstanceOf[Command.CancelOrder])

  private def processFirstPlacement(): Unit = placementQueue.dequeueOption.foreach {
    case (firstOrderId, _) =>
      pendingCommands.get(firstOrderId) match {
        case None =>
          throw new IllegalStateException(
            s"Can't find command for order $firstOrderId among pending commands: ${pendingCommands.keySet.mkString(", ")}")
        case Some(nextCommand) =>
          nextCommand.command match {
            case command: Command.PlaceOrder =>
              getTradableBalance(Set(command.order.getSpendAssetId, command.order.matcherFeeAssetId))
                .map { tradableBalance =>
                  val ao = command.toAcceptedOrder(tradableBalance)
                  OrderValidator.accountStateAware(ao.order.sender, tradableBalance, orderBookCache)(ao) match {
                    case Left(error) => Event.ValidationFailed(ao.order.id(), error)
                    case Right(_)    => Event.ValidationPassed(ao)
                  }
                }
                .pipeTo(self)
            case x => throw new IllegalStateException(s"Can't process $x, only PlaceOrder is allowed")
          }
      }
  }

  private def getTradableBalance(forAssets: Set[Asset]): Future[Map[Asset, Long]] =
    Future
      .traverse(forAssets)(asset => spendableBalance(asset).tupleLeft(asset))
      .map(_.toMap |-| openVolume)

  private def scheduleExpiration(order: Order): Unit = if (enableSchedules) {
    val timeToExpiration = (order.expiration - time.correctedTime()).max(0L)
    log.trace(s"Order ${order.id()} will expire in ${JDuration.ofMillis(timeToExpiration)}, at ${Instant.ofEpochMilli(order.expiration)}")
    expiration +=
      order.id() -> context.system.scheduler.scheduleOnce(timeToExpiration.millis, self, CancelExpiredOrder(order.id()))
  }

  private def handleOrderAdded(ao: AcceptedOrder): Unit = {
    log.trace(s"Saving order ${ao.order.id()}, new status is ${activeStatus(ao)}")
    orderDB.saveOrder(ao.order) // TODO do once when OrderAdded will be the first event. UP: it happens everytime orderbooks are restored, FIX this
    val origAoReservableBalance = activeOrders.get(ao.order.id()).fold(Map.empty[Asset, Long])(_.reservableBalance)
    val diff                    = (ao.reservableBalance |-| origAoReservableBalance).filter { case (_, v) => v != 0 }
    if (diff.nonEmpty) openVolume = openVolume |+| diff

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
    orderDB.saveOrderInfo(ao.order.id(), owner, OrderInfo.v2(ao.order, status))
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
          trySubtract(currentBalance, requiredBalance) match {
            case Some(updatedCurrentBalances) => updatedCurrentBalances -> ordersToCancel
            case None                         => currentBalance         -> ordersToCancel.enqueue(ao)
          }
      }
      ._2
  }

  private def place(ao: AcceptedOrder): Unit = {
    openVolume = openVolume |+| ao.reservableBalance
    activeOrders.put(ao.order.id(), ao)
    storeEvent(ao.order.id())(ao match {
      case ao: LimitOrder  => QueueEvent.Placed(ao)
      case ao: MarketOrder => QueueEvent.PlacedMarket(ao)
    })
  }

  private def cancel(o: AcceptedOrder): Unit =
    storeEvent(o.order.id())(QueueEvent.Canceled(o.order.assetPair, o.order.id()))

  private def storeEvent(orderId: Order.Id)(event: QueueEvent): Unit =
    store(event)
      .transform {
        case Success(None) => Success(Some(error.FeatureDisabled))
        case Success(_)    => Success(None)
        case _             => Success(Some(error.CanNotPersistEvent))
      }
      .onComplete {
        case Success(Some(error)) => self ! Event.StoreFailed(orderId, error)
        case _                    =>
      }

  private def hasOrder(id: Order.Id): Boolean =
    pendingCommands.contains(id) || activeOrders.contains(id) || orderDB.containsInfo(id) || hasOrderInBlockchain(id)

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
  private def trySubtract(currentBalance: Map[Asset, Long], requiredBalance: Map[Asset, Long]): Option[Map[Asset, Long]] = {
    requiredBalance.foldLeft[Option[Map[Asset, Long]]] { Some(currentBalance) } {
      case (None, _)                => None
      case (currentBalance, (_, 0)) => currentBalance
      case (Some(currentBalance), (assetId, requiredAmount)) =>
        val updatedAmount = currentBalance.getOrElse(assetId, 0L) - requiredAmount
        if (updatedAmount < 0) None
        else Some { currentBalance.updated(assetId, updatedAmount) }
    }
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
    case class CancelNotEnoughCoinsOrders(newBalance: Map[Asset, Long])  extends Command
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
}
