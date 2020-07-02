package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrder(id: Order.Id,
                   timestamp: Option[Long] = None,
                   amountAsset: Option[Asset] = None,
                   priceAsset: Option[Asset] = None,
                   side: Option[OrderType] = None,
                   isMarket: Option[Boolean] = None,
                   price: Option[Double] = None,
                   amount: Option[Double] = None,
                   fee: Option[Double] = None,
                   feeAsset: Option[Asset] = None,
                   status: Option[String] = None,
                   filledAmount: Option[Double] = None,
                   filledFee: Option[Double] = None,
                   avgWeighedPrice: Option[Double] = None)

object WsOrder {

  def fromDomain(ao: AcceptedOrder)(implicit efc: ErrorFormatterContext): WsOrder = fromDomain(ao, ao.status)

  def fromDomain(ao: AcceptedOrder, status: OrderStatus)(implicit efc: ErrorFormatterContext): WsOrder = {

    val amountAssetDecimals = efc.unsafeAssetDecimals(ao.order.assetPair.amountAsset)
    val feeAssetDecimals    = efc.unsafeAssetDecimals(ao.order.feeAsset)
    val priceAssetDecimals  = efc.unsafeAssetDecimals(ao.order.assetPair.priceAsset)

    def denormalizeAmount(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizeFee(value: Long): Double    = Denormalization.denormalizeAmountAndFee(value, feeAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double  = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    WsOrder(
      ao.id,
      ao.order.timestamp.some,
      ao.order.assetPair.amountAsset.some,
      ao.order.assetPair.priceAsset.some,
      ao.order.orderType.some,
      ao.isMarket.some,
      ao.price.some.map(denormalizePrice),
      ao.order.amount.some.map(denormalizeAmount),
      ao.order.matcherFee.some.map(denormalizeFee),
      ao.feeAsset.some,
      status.name.some,
      ao.fillingInfo.filledAmount.some.map(denormalizeAmount),
      ao.fillingInfo.filledFee.some.map(denormalizeFee),
      ao.fillingInfo.avgWeighedPrice.some.map(denormalizePrice)
    )
  }

  def apply(id: Order.Id, status: String, filledAmount: Double, filledFee: Double, avgWeighedPrice: Double): WsOrder = {
    WsOrder(id, status = status.some, filledAmount = filledAmount.some, filledFee = filledFee.some, avgWeighedPrice = avgWeighedPrice.some)
  }

  def apply(id: Order.Id, status: String): WsOrder = WsOrder(id, status = status.some)

  val isMarketFormat: Format[Boolean] = Format(
    {
      case JsString("MARKET") => JsSuccess(true)
      case JsString("LIMIT")  => JsSuccess(false)
      case _                  => JsError("Cannot parse order type (MARKET or LIMIT)!")
    },
    if (_) JsString("MARKET") else JsString("LIMIT")
  )

  object Status extends Enumeration {
    type Status = Value
    val ACCEPTED, FILLED, PARTIALLY_FILLED, CANCELLED = Value
  }

  val orderStatusFormat: Format[String] = Format(
    {
      case JsString(s) if s == Status.ACCEPTED.toString         => JsSuccess(OrderStatus.Accepted.name)
      case JsString(s) if s == Status.FILLED.toString           => JsSuccess(OrderStatus.Filled.name)
      case JsString(s) if s == Status.PARTIALLY_FILLED.toString => JsSuccess(OrderStatus.PartiallyFilled.name)
      case JsString(s) if s == Status.CANCELLED.toString        => JsSuccess(OrderStatus.Cancelled.name)
      case _                                                    => JsError("Cannot parse order status")
    }, {
      case OrderStatus.Accepted.name        => JsString(Status.ACCEPTED.toString)
      case OrderStatus.Filled.name          => JsString(Status.FILLED.toString)
      case OrderStatus.PartiallyFilled.name => JsString(Status.PARTIALLY_FILLED.toString)
      case OrderStatus.Cancelled.name       => JsString(Status.CANCELLED.toString)
    }
  )

  implicit val wsOrderFormat: Format[WsOrder] =
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
        (__ \ "r").formatNullable[Double](doubleAsStringFormat)     // average weighed price among all trades
    )(WsOrder.apply, unlift(WsOrder.unapply))
}
