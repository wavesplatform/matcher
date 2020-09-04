package com.wavesplatform.dex.history

import java.time.{Instant, LocalDateTime, ZoneOffset}

import akka.actor.{Actor, ActorRef, Props}
import cats.syntax.option._
import com.typesafe.config.Config
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord, Record}
import com.wavesplatform.dex.history.HistoryRouterActor.HistoryInsertMsg._
import com.wavesplatform.dex.history.HistoryRouterActor.HistoryUpdateMsg._
import com.wavesplatform.dex.history.HistoryRouterActor._
import com.wavesplatform.dex.model.Events._
import com.wavesplatform.dex.model.OrderStatus.Filled
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}

object HistoryRouterActor {

  def props(assetDecimals: Asset => Int, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings): Props =
    Props(new HistoryRouterActor(assetDecimals, postgresConnection, orderHistorySettings))

  def toLocalDateTime(timestamp: Long): LocalDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneOffset.UTC)

  val eventTrade, buySide, limitOrderType    = 0: Byte
  val eventCancel, sellSide, marketOrderType = 1: Byte

  val statusPartiallyFilled: Byte = 1
  val statusFilled: Byte          = 2
  val statusCancelled: Byte       = 3

  sealed trait HistoryMsg extends Product with Serializable

  sealed trait HistoryInsertMsg extends HistoryMsg {

    type R <: Record // mapping between domain objects and database rows
    type DenormalizePrice        = (Long, AssetPair) => BigDecimal // how to convert price to the human-readable format
    type DenormalizeAmountAndFee = (Long, Asset) => BigDecimal     // how to convert amount and fee fee to the human-readable format

    protected def createRecords(denormalizeAmountAndFee: DenormalizeAmountAndFee, denormalizePrice: DenormalizePrice): Set[R]
  }

  object HistoryInsertMsg {

    final case class SaveOrder(acceptedOrder: AcceptedOrder, timestamp: Long) extends HistoryInsertMsg {

      type R = OrderRecord

      def createRecords(denormalizeAmountAndFee: DenormalizeAmountAndFee, denormalizePrice: DenormalizePrice): Set[R] = {
        val order = this.acceptedOrder.order
        Set(
          OrderRecord(
            id = order.id().toString,
            tpe = if (acceptedOrder.isMarket) marketOrderType else limitOrderType,
            senderAddress = order.sender.toAddress.toString,
            senderPublicKey = order.senderPublicKey.toString,
            amountAssetId = order.assetPair.amountAssetStr,
            priceAssetId = order.assetPair.priceAssetStr,
            feeAssetId = order.feeAsset.toString,
            side = if (acceptedOrder.isBuyOrder) buySide else sellSide,
            price = denormalizePrice(order.price, order.assetPair),
            amount = denormalizeAmountAndFee(order.amount, order.assetPair.amountAsset),
            timestamp = toLocalDateTime(order.timestamp),
            expiration = toLocalDateTime(order.expiration),
            fee = denormalizeAmountAndFee(order.matcherFee, order.feeAsset),
            created = toLocalDateTime(this.timestamp),
            closedAt = None
          )
        )
      }
    }

    final case class SaveEvent(event: Event) extends HistoryInsertMsg {

      override type R = EventRecord

      override def createRecords(denormalizeAmountAndFee: DenormalizeAmountAndFee, denormalizePrice: DenormalizePrice): Set[R] = {
        this.event match {
          case _: OrderAdded => Set.empty[EventRecord]

          case e @ OrderExecuted(submitted, counter, timestamp, _, _) =>
            val assetPair = submitted.order.assetPair

            Set(
              (submitted, e.submittedRemainingAmount, e.submittedExecutedFee, e.submittedRemainingFee, e.submittedRemaining),
              (counter, e.counterRemainingAmount, e.counterExecutedFee, e.counterRemainingFee, e.counterRemaining)
            ) map {
              case (acceptedOrder, remainingAmount, executedFee, remainingFee, remaining) =>
                EventRecord(
                  orderId = acceptedOrder.order.id().toString,
                  eventType = eventTrade,
                  timestamp = toLocalDateTime(timestamp),
                  price = denormalizePrice(acceptedOrder.order.price, assetPair),
                  filled = denormalizeAmountAndFee(e.executedAmount, assetPair.amountAsset),
                  totalFilled = denormalizeAmountAndFee(acceptedOrder.order.amount - remainingAmount, assetPair.amountAsset),
                  feeFilled = denormalizeAmountAndFee(executedFee, acceptedOrder.order.feeAsset),
                  feeTotalFilled = denormalizeAmountAndFee(acceptedOrder.order.matcherFee - remainingFee, acceptedOrder.order.feeAsset),
                  status = if (remaining.isFilled) statusFilled else statusPartiallyFilled,
                  reason = e.reason
                )
            }

          case OrderCanceled(submitted, reason, timestamp) =>
            val assetPair = submitted.order.assetPair
            Set(
              EventRecord(
                orderId = submitted.order.id().toString,
                eventType = eventCancel,
                timestamp = toLocalDateTime(timestamp),
                price = denormalizePrice(submitted.order.price, assetPair),
                filled = 0,
                totalFilled = denormalizeAmountAndFee(submitted.order.amount - submitted.amount, assetPair.amountAsset),
                feeFilled = 0,
                feeTotalFilled = denormalizeAmountAndFee(submitted.order.matcherFee - submitted.fee, submitted.order.feeAsset),
                status = OrderStatus.finalCancelStatus(submitted, reason) match { case _: Filled => statusFilled; case _ => statusCancelled },
                reason = reason
              )
            )
        }
      }
    }
  }

  sealed trait HistoryUpdateMsg extends HistoryMsg

  object HistoryUpdateMsg {
    final case class UpdateOrder(id: String, closedAt: Option[LocalDateTime]) extends HistoryUpdateMsg
  }

  final case object StopAccumulate

}

class HistoryRouterActor(assetDecimals: Asset => Int, postgresConnection: PostgresConnection, orderHistorySettings: OrderHistorySettings)
    extends Actor
    with HasPostgresJdbcContext {

  import ctx._

  private def denormalizeAmountAndFee(value: Long, asset: Asset): BigDecimal =
    Denormalization.denormalizeAmountAndFee(value, assetDecimals(asset))

  private def denormalizePrice(value: Long, pair: AssetPair): BigDecimal =
    Denormalization.denormalizePrice(value, assetDecimals(pair.amountAsset), assetDecimals(pair.priceAsset))

  override def connectionConfig: Config = postgresConnection.getConfig

  private val ordersHistory: ActorRef = context.actorOf(
    Props(
      new HistoryMessagesBatchSenderActor[HistoryMsg] {

        val batchLinger: Long  = orderHistorySettings.ordersBatchLingerMs
        val batchEntries: Long = orderHistorySettings.ordersBatchEntries

        def toRecord(saveOrder: SaveOrder): OrderRecord = saveOrder.createRecords(denormalizeAmountAndFee, denormalizePrice).head

        override def createAndSendBatch(batchBuffer: Iterable[HistoryMsg]): Unit = {

          val (updates, newOrders) = batchBuffer.foldLeft { (List.empty[UpdateOrder], Map.empty[String, OrderRecord]) } {
            case ((updates, newOrders), msg) =>
              msg match {
                case _: SaveEvent        => updates -> newOrders // Impossible here
                case newOrder: SaveOrder => updates -> newOrders.updated(newOrder.acceptedOrder.id.base58, toRecord(newOrder))
                case update @ UpdateOrder(updatedOrderId, closedAt) =>
                  newOrders.get(updatedOrderId) match {
                    case None        => (update :: updates) -> newOrders
                    case Some(order) => updates             -> newOrders.updated(updatedOrderId, order.copy(closedAt = closedAt))
                  }
              }
          }

          ctx.run {
            liftQuery(newOrders.values) foreach { orderRecord =>
              querySchema[OrderRecord](
                "orders",
                _.id              -> "id",
                _.tpe             -> "type",
                _.senderAddress   -> "sender_address",
                _.senderPublicKey -> "sender_public_key",
                _.amountAssetId   -> "amount_asset_id",
                _.priceAssetId    -> "price_asset_id",
                _.feeAssetId      -> "fee_asset_id",
                _.side            -> "side",
                _.price           -> "price",
                _.amount          -> "amount",
                _.timestamp       -> "timestamp",
                _.expiration      -> "expiration",
                _.fee             -> "fee",
                _.created         -> "created",
                _.closedAt        -> "closed_at"
              ).insert(orderRecord).onConflictIgnore
            }
          }

          ctx.run {
            quote {
              liftQuery(updates) foreach { x =>
                querySchema[OrderRecord]("orders")
                  .filter(_.id == x.id)
                  .update(_.closedAt -> x.closedAt)
              }
            }
          }
        }
      }
    ),
    name = "orders-history"
  )

  private val eventsHistory: ActorRef = context.actorOf(
    Props(
      new HistoryMessagesBatchSenderActor[SaveEvent] {

        val batchLinger: Long  = orderHistorySettings.eventsBatchLingerMs
        val batchEntries: Long = orderHistorySettings.eventsBatchEntries

        def createAndSendBatch(batchBuffer: Iterable[SaveEvent]): Unit =
          ctx.run {
            liftQuery(batchBuffer flatMap { _.createRecords(denormalizeAmountAndFee, denormalizePrice) }) foreach { eventRecord =>
              querySchema[EventRecord](
                "events",
                _.orderId        -> "order_id",
                _.eventType      -> "event_type",
                _.timestamp      -> "timestamp",
                _.price          -> "price",
                _.filled         -> "filled",
                _.totalFilled    -> "total_filled",
                _.feeFilled      -> "fee_filled",
                _.feeTotalFilled -> "fee_total_filled",
                _.status         -> "status",
                _.reason         -> "reason"
              ).insert(eventRecord).onConflictIgnore
            }
          }
      }
    ),
    name = "events-history"
  )

  private def updateOrderClosedAtTimestamp(event: Event): Unit = {

    val ids2ts = event match {
      case OrderCanceled(ao, _, timestamp) => Map(ao.id -> timestamp)
      case e @ OrderExecuted(_, _, timestamp, _, _) => // looking for filled orders
        Seq(e.submittedRemaining, e.counterRemaining).foldLeft(Map.empty[Order.Id, Long]) {
          case (result, ao) => if (ao.isFilled) result + (ao.id -> timestamp) else result
        }
      case _ => Map.empty[Order.Id, Long]
    }

    ids2ts.foreach { case (orderId, closedAt) => ordersHistory ! UpdateOrder(orderId.base58, toLocalDateTime(closedAt).some) }
  }

  def receive: Receive = {
    case newOrder: SaveOrder => ordersHistory forward newOrder
    case newEvent: SaveEvent => eventsHistory forward newEvent; updateOrderClosedAtTimestamp(newEvent.event)
  }
}
