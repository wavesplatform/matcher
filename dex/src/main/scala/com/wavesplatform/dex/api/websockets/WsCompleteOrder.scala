package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsCompleteOrder(id: Order.Id,
                           timestamp: Long,
                           amountAsset: Asset,
                           priceAsset: Asset,
                           side: OrderType,
                           isMarket: Boolean,
                           price: Double,
                           amount: Double,
                           fee: Double,
                           feeAsset: Asset,
                           status: String,
                           filledAmount: Double,
                           filledFee: Double,
                           avgWeighedPrice: Double,
                           eventTimestamp: Long,
                           executedAmount: Option[Double] = None,
                           executedFee: Option[Double] = None,
                           executedPrice: Option[Double] = None)

object WsCompleteOrder {

  def from(event: OrderCanceled)(implicit efc: ErrorFormatterContext): WsCompleteOrder = {
    val ao = event.acceptedOrder

    val amountAssetDecimals = efc.assetDecimals(ao.order.assetPair.amountAsset)
    val priceAssetDecimals  = efc.assetDecimals(ao.order.assetPair.priceAsset)

    def denormalizeAmountAndFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double        = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    WsCompleteOrder(
      id = ao.id,
      timestamp = ao.order.timestamp,
      amountAsset = ao.order.assetPair.amountAsset,
      priceAsset = ao.order.assetPair.priceAsset,
      side = ao.order.orderType,
      isMarket = ao.isMarket,
      price = denormalizePrice(ao.order.price),
      amount = denormalizeAmountAndFee(ao.order.amount),
      fee = denormalizeAmountAndFee(ao.order.matcherFee),
      feeAsset = ao.order.feeAsset,
      status = OrderStatus.Cancelled.name,
      filledAmount = denormalizeAmountAndFee(ao.order.amount),
      filledFee = denormalizeAmountAndFee(ao.order.matcherFee),
      avgWeighedPrice = denormalizePrice(ao.fillingInfo.avgWeighedPrice),
      eventTimestamp = event.timestamp,
      executedAmount = none,
      executedFee = none,
      executedPrice = none
    )
  }

  def from(ao: AcceptedOrder, event: OrderExecuted, denormalizeAmountAndFee: Long => Double, denormalizePrice: Long => Double)(
      implicit efc: ErrorFormatterContext): WsCompleteOrder = WsCompleteOrder(
    id = ao.id,
    timestamp = ao.order.timestamp,
    amountAsset = ao.order.assetPair.amountAsset,
    priceAsset = ao.order.assetPair.priceAsset,
    side = ao.order.orderType,
    isMarket = ao.isMarket,
    price = denormalizePrice(ao.order.price),
    amount = denormalizeAmountAndFee(ao.order.amount),
    fee = denormalizeAmountAndFee(ao.order.matcherFee),
    feeAsset = ao.order.feeAsset,
    status = ao.status.name,
    filledAmount = denormalizeAmountAndFee(ao.fillingInfo.filledAmount),
    filledFee = denormalizeAmountAndFee(ao.fillingInfo.filledFee),
    avgWeighedPrice = denormalizePrice(ao.fillingInfo.avgWeighedPrice),
    eventTimestamp = event.timestamp,
    executedAmount = denormalizeAmountAndFee(event.executedAmount).some,
    executedFee = denormalizeAmountAndFee(event.counterExecutedFee).some,
    executedPrice = denormalizePrice(event.executedPrice).some
  )

  private val isMarketFormat: Format[Boolean] = Format(
    {
      case JsString("market") => JsSuccess(true)
      case JsString("limit")  => JsSuccess(false)
      case _                  => JsError("Cannot parse order type (market or limit)!")
    },
    if (_) JsString("market") else JsString("limit")
  )

  object Status extends Enumeration {
    type Status = Value
    val Filled, PartiallyFilled, Cancelled = Value
  }

  private val orderStatusFormat: Format[String] = Format(
    {
      case JsString(s) if s == Status.Filled.toString          => JsSuccess(OrderStatus.Filled.name)
      case JsString(s) if s == Status.PartiallyFilled.toString => JsSuccess(OrderStatus.PartiallyFilled.name)
      case JsString(s) if s == Status.Cancelled.toString       => JsSuccess(OrderStatus.Cancelled.name)
      case _                                                   => JsError("Cannot parse order status")
    }, {
      case OrderStatus.Filled.name          => JsString(Status.Filled.toString)
      case OrderStatus.PartiallyFilled.name => JsString(Status.PartiallyFilled.toString)
      case OrderStatus.Cancelled.name       => JsString(Status.Cancelled.toString)
    }
  )

  implicit val wsCompleteOrderFormat: Format[WsCompleteOrder] =
    (
      (__ \ "i").format[Order.Id] and                               // id
        (__ \ "t").format[Long] and                                 // timestamp
        (__ \ "A").format[Asset] and                                // amount asset
        (__ \ "P").format[Asset] and                                // price asset
        (__ \ "S").format[OrderType] and                            // side: BUY or SELL
        (__ \ "T").format[Boolean](isMarketFormat) and              // type: MARKET or LIMIT
        (__ \ "p").format[Double](doubleAsStringFormat) and         // price
        (__ \ "a").format[Double](doubleAsStringFormat) and         // amount
        (__ \ "f").format[Double](doubleAsStringFormat) and         // fee
        (__ \ "F").format[Asset] and                                // fee asset
        (__ \ "s").format[String](orderStatusFormat) and            // status: FILLED or PARTIALLY_FILLED or CANCELLED
        (__ \ "q").format[Double](doubleAsStringFormat) and         // filled amount
        (__ \ "Q").format[Double](doubleAsStringFormat) and         // filled fee
        (__ \ "r").format[Double](doubleAsStringFormat) and         // average weighed price
        (__ \ "Z").format[Long] and                                 // event timestamp
        (__ \ "c").formatNullable[Double](doubleAsStringFormat) and // executed amount
        (__ \ "h").formatNullable[Double](doubleAsStringFormat) and // executed fee
        (__ \ "e").formatNullable[Double](doubleAsStringFormat)     // executed price
    )(WsCompleteOrder.apply, unlift(WsCompleteOrder.unapply))
}
