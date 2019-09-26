package com.wavesplatform.dex

import java.time.{Instant, Duration => JDuration}

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.pipe
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.Matcher.StoreEvent
import com.wavesplatform.dex.api.NotImplemented
import com.wavesplatform.dex.db.OrderDB
import com.wavesplatform.dex.db.OrderDB.orderInfoOrdering
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair.assetIdStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{LoggerFacade, ScorexLogging, Time}
import mouse.any._
import org.slf4j.LoggerFactory
import cats.implicits._
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.market.{BalanceActor, PlaceValidatorActor}
import com.wavesplatform.dex.util.WorkingStash

import scala.collection.immutable.Queue
import scala.collection.mutable.{AnyRefMap => MutableMap}
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/*
New order (requires balances):
1. [AddressActor] Place[Market|Limit]Order
2. [AddressActor] Getting balances
3. [AddressActor] Validation
4. [AddressActor] Reserving
5. [AddressActor] Storing, adding to pendingPlacement
6. [OrderBookActor] Processing
7. [OrderBookActor] Sending an event to AddressActor
8. [AddressActor] Accepting, resolving in pendingPlacement
9. [AddressActor] Sending a response to the client

Cancel order:
1. [AddressActor] CancelOrder
2. [AddressActor] Validation
3. [AddressActor] Storing, adding to pendingCancellation
4. [OrderBookActor] Processing
5. [OrderBookActor] Sending an event to AddressActor
6. [AddressActor] Accepting, resolving in pendingCancellation
7. [AddressActor] Sending a response to the client

Get[Tradabale|Reserved]Balance
GetOrderStatus
 */
class NewAddressActor(owner: Address,
                      balancesActor: ActorRef,
                      storeActor: ActorRef,
                      time: Time,
                      orderDB: OrderDB,
                      hasOrder: Order.Id => Boolean,
                      orderBookCache: AssetPair => OrderBook.AggregatedSnapshot,
                      var enableSchedules: Boolean)
    extends Actor
    with ScorexLogging {

  import NewAddressActor._

  protected override lazy val log      = LoggerFacade(LoggerFactory.getLogger(s"AddressActor[$owner]"))
  private val validatorActor: ActorRef = context.actorOf(Props(classOf[PlaceValidatorActor], self, balancesActor), "validator")

  private val processingCommands = MutableMap.empty[Order.Id, Item]
  private val activeOrders       = MutableMap.empty[Order.Id, AcceptedOrder]

  override def receive: Receive = {
    case command: PlaceOrder =>
      val orderId = command.order.id()
      if (processingCommands.contains(orderId)) sender ! api.OrderRejected(error.OrderDuplicate(orderId))
      else {
        processingCommands.put(orderId, Item(command, sender))
        validatorActor ! command
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
            if (ao.isLimit) {
              processingCommands.put(orderId, Item(command, sender))
              storeActor ! QueueEvent.Canceled(ao.order.assetPair, orderId)
            } else sender ! api.OrderCancelRejected(error.MarketOrderCancel(orderId))
        }

    case command: CancelAllOrders        => // actor with sending all cancels?
    case command: CancelExpiredOrder     =>
    case AddressDirectory.StartSchedules =>
    case event: OrderEvent               => applyEvent(event)
  }

  private def applyEvent(event: OrderEvent): Unit = {
    activeOrders.get(event.orderId).foreach { order =>
      event match {
        case OrderEvent.Accepted(id) => // ?
        case OrderEvent.Executed(id) =>
        case OrderEvent.Canceled(id) =>
        case x                       =>
      }
    }

    processingCommands.get(event.orderId).foreach { item =>
      item.command match {
        case command: PlaceOrder  => applyEvent(command, event, item.client)
        case command: CancelOrder => applyEvent(command, event, item.client)
      }
    }
  }

  private def applyEvent(place: PlaceOrder, event: Any, client: ActorRef): Unit = event match {
    case OrderEvent.ValidationFailed(id, reason) =>
      processingCommands.remove(id)
      client ! api.OrderRejected(reason)

    case OrderEvent.ValidationPassed(id) =>
      balancesActor ! BalancesActor.Command.Reserve(place.order.sender, ???)
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
}

object NewAddressActor {

  case class Item(command: Command, client: ActorRef)

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
    override lazy val toString = s"PlaceLimitOrder(${getOrderInfo(order)})"
  }

  case class CancelOrder(orderId: ByteStr)                             extends Command
  case class CancelAllOrders(pair: Option[AssetPair], timestamp: Long) extends Command
  case class BalanceUpdated(actualBalance: Map[Asset, Long])           extends Command

  private case class CancelExpiredOrder(orderId: ByteStr)

  sealed trait OrderEvent {
    def orderId: Order.Id
  }

  object OrderEvent {
    case class ValidationFailed(orderId: Order.Id, reason: MatcherError) extends OrderEvent
    case class ValidationPassed(orderId: Order.Id)                       extends OrderEvent

    case class StoringFailed(orderId: Order.Id, reason: MatcherError) extends OrderEvent
    case class StoringPassed(orderId: Order.Id)                       extends OrderEvent

    case class Accepted(orderId: Order.Id) extends OrderEvent
    case class Executed(orderId: Order.Id) extends OrderEvent
    case class Canceled(orderId: Order.Id) extends OrderEvent
  }
}
