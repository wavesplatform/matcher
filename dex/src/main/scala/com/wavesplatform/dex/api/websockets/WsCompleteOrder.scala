package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
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

  def fromDomain(ao: AcceptedOrder, status: OrderStatus)(implicit efc: ErrorFormatterContext): WsCompleteOrder = {

    val amountAssetDecimals = efc.assetDecimals(ao.order.assetPair.amountAsset)
    val priceAssetDecimals  = efc.assetDecimals(ao.order.assetPair.priceAsset)

    def denormalizeAmountAndFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double        = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    WsCompleteOrder(
      ao.id,
      ao.order.timestamp,
      ao.order.assetPair.amountAsset,
      ao.order.assetPair.priceAsset,
      ao.order.orderType,
      ao.isMarket,
      denormalizePrice(ao.price),
      denormalizeAmountAndFee(ao.order.amount),
      denormalizeAmountAndFee(ao.order.matcherFee),
      ao.feeAsset,
      status.name,
      ao.fillingInfo.filledAmount.some.map(denormalizeAmountAndFee),
      ao.fillingInfo.filledFee.some.map(denormalizeAmountAndFee),
      ao.fillingInfo.avgWeighedPrice.some.map(denormalizePrice)
    )
  }

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
        (__ \ "t").formatNullable[Long] and                         // timestamp
        (__ \ "A").formatNullable[Asset] and                        // amount asset
        (__ \ "P").formatNullable[Asset] and                        // price asset
        (__ \ "S").formatNullable[OrderType] and                    // side: BUY or SELL
        (__ \ "T").formatNullable[Boolean](isMarketFormat) and      // type: MARKET or LIMIT
        (__ \ "p").formatNullable[Double](doubleAsStringFormat) and // price
        (__ \ "a").formatNullable[Double](doubleAsStringFormat) and // amount
        (__ \ "f").formatNullable[Double](doubleAsStringFormat) and // fee
        (__ \ "F").formatNullable[Asset] and                        // fee asset
        (__ \ "s").formatNullable[String](orderStatusFormat) and    // status: ACCEPTED or FILLED or PARTIALLY_FILLED or CANCELLED
        (__ \ "q").formatNullable[Double](doubleAsStringFormat) and // filled amount
        (__ \ "Q").formatNullable[Double](doubleAsStringFormat) and // filled fee
        (__ \ "r").formatNullable[Double](doubleAsStringFormat) and // average weighed price
        (__ \ "Z").format[Long] and                                 // event timestamp
        (__ \ "c").formatNullable[Double](doubleAsStringFormat) and // executed amount
        (__ \ "h").formatNullable[Double](doubleAsStringFormat) and // executed fee
        (__ \ "e").formatNullable[Double](doubleAsStringFormat) // executed price
    )(WsCompleteOrder.apply, unlift(WsCompleteOrder.unapply))
}
