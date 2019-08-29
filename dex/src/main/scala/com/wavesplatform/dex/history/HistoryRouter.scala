package com.wavesplatform.dex.history

import java.time.{Instant, LocalDateTime, ZoneOffset}

import akka.actor.{Actor, ActorRef, Props}
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord, Record}
import com.wavesplatform.dex.history.HistoryRouter.{SaveEvent, SaveOrder}
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.MatcherModel.Denormalization
import com.wavesplatform.dex.model.OrderStatus.Filled
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.assets.exchange.AssetPair
import io.getquill.{PostgresJdbcContext, SnakeCase}

object HistoryRouter {

  def props(blockchain: Blockchain, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings): Props =
    Props(new HistoryRouter(blockchain, postgresConnection, orderHistorySettings))

  val eventTrade, buySide, limitOrderType    = 0: Byte
  val eventCancel, sellSide, marketOrderType = 1: Byte

  val statusPartiallyFilled: Byte = 1
  val statusFilled: Byte          = 2
  val statusCancelled: Byte       = 3

  trait HistoryMsg {

    type R <: Record // mapping between domain objects and database rows
    type Denormalize = (Long, AssetPair) => Double // how to convert amount, price and fee to the human-readable format

    protected def createRecords(denormalizeAmountAndFee: Denormalize, denormalizePrice: Denormalize): Set[R]
    protected def toLocalDateTime(timestamp: Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneOffset.UTC)
  }

  final case class SaveOrder(acceptedOrder: AcceptedOrder, timestamp: Long) extends HistoryMsg {

    type R = OrderRecord

    def createRecords(denormalizeAmountAndFee: Denormalize, denormalizePrice: Denormalize): Set[R] = {
      val order = this.acceptedOrder.order
      Set(
        OrderRecord(
          id = order.id().toString,
          tpe = if (acceptedOrder.isMarket) marketOrderType else limitOrderType,
          senderAddress = order.sender.toAddress.toString,
          senderPublicKey = order.senderPublicKey.toString,
          amountAssetId = order.assetPair.amountAssetStr,
          priceAssetId = order.assetPair.priceAssetStr,
          feeAssetId = AssetPair.assetIdStr(order.matcherFeeAssetId),
          side = if (acceptedOrder.isBuyOrder) buySide else sellSide,
          price = denormalizePrice(order.price, order.assetPair),
          amount = denormalizeAmountAndFee(order.amount, order.assetPair),
          timestamp = toLocalDateTime(order.timestamp),
          expiration = toLocalDateTime(order.expiration),
          fee = denormalizeAmountAndFee(order.matcherFee, order.assetPair),
          created = toLocalDateTime(this.timestamp)
        )
      )
    }
  }

  final case class SaveEvent(event: Event) extends HistoryMsg {

    type R = EventRecord

    def createRecords(denormalizeAmountAndFee: Denormalize, denormalizePrice: Denormalize): Set[R] = {
      this.event match {
        case _: OrderAdded => Set.empty[EventRecord]

        case e @ OrderExecuted(submitted, counter, timestamp) =>
          val assetPair = submitted.order.assetPair

          Set(
            (submitted, e.submittedRemainingAmount, e.submittedExecutedFee, e.submittedRemainingFee),
            (counter, e.counterRemainingAmount, e.counterExecutedFee, e.counterRemainingFee)
          ) map {
            case (acceptedOrder, remainingAmount, executedFee, remainingFee) =>
              EventRecord(
                orderId = acceptedOrder.order.id().toString,
                eventType = eventTrade,
                timestamp = toLocalDateTime(timestamp),
                price = denormalizePrice(acceptedOrder.order.price, assetPair),
                filled = denormalizeAmountAndFee(e.executedAmount, assetPair),
                totalFilled = denormalizeAmountAndFee(acceptedOrder.order.amount - remainingAmount, assetPair),
                feeFilled = denormalizeAmountAndFee(executedFee, assetPair),
                feeTotalFilled = denormalizeAmountAndFee(acceptedOrder.order.matcherFee - remainingFee, assetPair),
                status = if (remainingAmount == 0) statusFilled else statusPartiallyFilled
              )
          }

        case OrderCanceled(submitted, isSystemCancel, timestamp) =>
          val assetPair = submitted.order.assetPair
          Set(
            EventRecord(
              orderId = submitted.order.id().toString,
              eventType = eventCancel,
              timestamp = toLocalDateTime(timestamp),
              price = denormalizePrice(submitted.order.price, assetPair),
              filled = 0,
              totalFilled = denormalizeAmountAndFee(submitted.order.amount - submitted.amount, assetPair),
              feeFilled = 0,
              feeTotalFilled = denormalizeAmountAndFee(submitted.order.matcherFee - submitted.fee, assetPair),
              status = OrderStatus.finalStatus(submitted, isSystemCancel) match { case _: Filled => statusFilled; case _ => statusCancelled }
            )
          )
      }
    }
  }

  final case object StopAccumulate
}

class HistoryRouter(blockchain: Blockchain, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings) extends Actor {

  private def denormalizeAmountAndFee(value: Long, pair: AssetPair): Double =
    Denormalization.denormalizeAmountAndFeeWithDefault(value, pair, blockchain)

  private def denormalizePrice(value: Long, pair: AssetPair): Double =
    Denormalization.denormalizePriceWithDefault(value, pair, blockchain)

  private val ctx = new PostgresJdbcContext(SnakeCase, postgresConnection.getConfig); import ctx._

  private val ordersHistory: ActorRef = context.actorOf(
    Props(
      new HistoryMessagesBatchSender[SaveOrder] {

        val batchLinger: Long  = orderHistorySettings.ordersBatchLingerMs
        val batchEntries: Long = orderHistorySettings.ordersBatchEntries

        def createAndSendBatch(batchBuffer: Iterable[SaveOrder]): Unit =
          ctx.run {
            liftQuery(batchBuffer flatMap { _.createRecords(denormalizeAmountAndFee, denormalizePrice) }) foreach { orderRecord =>
              querySchema[OrderRecord](
                "orders",
                _.id              -> "id",
                _.tpe             -> "type",
                _.senderAddress   -> "sender_address",
                _.senderPublicKey -> "sender_public_key",
                _.amountAssetId   -> "amount_asset_id",
                _.priceAssetId    -> "price_asset_id",
                _.side            -> "side",
                _.price           -> "price",
                _.amount          -> "amount",
                _.timestamp       -> "timestamp",
                _.expiration      -> "expiration",
                _.fee             -> "fee",
                _.created         -> "created"
              ).insert(orderRecord).onConflictIgnore
            }
          }
      }
    ),
    name = "orders-history"
  )

  private val eventsHistory: ActorRef = context.actorOf(
    Props(
      new HistoryMessagesBatchSender[SaveEvent] {

        val batchLinger: Long  = orderHistorySettings.eventsBatchLingerMs
        val batchEntries: Long = orderHistorySettings.eventsBatchEntries

        def createAndSendBatch(batchBuffer: Iterable[SaveEvent]): Unit =
          ctx.run {
            liftQuery(batchBuffer flatMap { _.createRecords(denormalizeAmountAndFee, denormalizePrice) }) foreach { eventRecord =>
              querySchema[EventRecord](
                "events",
                _.orderId     -> "order_id",
                _.eventType   -> "event_type",
                _.timestamp   -> "timestamp",
                _.price       -> "price",
                _.filled      -> "filled",
                _.totalFilled -> "total_filled",
                _.status      -> "status"
              ).insert(eventRecord).onConflictIgnore
            }
          }
      }
    ),
    name = "events-history"
  )

  def receive: Receive = {
    case newOrder: SaveOrder => ordersHistory forward newOrder
    case newEvent: SaveEvent => eventsHistory forward newEvent
  }
}
