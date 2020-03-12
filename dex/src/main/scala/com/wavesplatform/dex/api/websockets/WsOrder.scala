package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrder(id: Order.Id,
                   timestamp: Option[Long] = None,
                   amountAsset: Option[Asset] = None,
                   priceAsset: Option[Asset] = None,
                   side: Option[OrderType] = None,
                   isMarket: Option[Boolean] = None,
                   price: Option[Long] = None,
                   amount: Option[Long] = None,
                   fee: Option[Long] = None,
                   feeAsset: Option[Asset] = None,
                   status: Option[String] = None,
                   filledAmount: Option[Long] = None,
                   filledFee: Option[Long] = None,
                   avgFilledPrice: Option[Long] = None)

object WsOrder {

  def fromDomain(ao: AcceptedOrder, status: OrderStatus): WsOrder =
    WsOrder(
      ao.id,
      ao.order.timestamp.some,
      ao.order.assetPair.amountAsset.some,
      ao.order.assetPair.priceAsset.some,
      ao.order.orderType.some,
      ao.isMarket.some,
      ao.price.some,
      ao.order.amount.some,
      ao.order.matcherFee.some,
      ao.feeAsset.some,
      status.name.some,
      ao.fillingInfo.filledAmount.some,
      ao.fillingInfo.filledFee.some,
      ao.fillingInfo.avgFilledPrice.some
    )

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

  implicit val format: Format[WsOrder] =
    (
      (JsPath \ "i").format[Order.Id] and                            // id
        (JsPath \ "t").formatNullable[Long] and                      // timestamp
        (JsPath \ "A").formatNullable[Asset] and                     // amount asset
        (JsPath \ "P").formatNullable[Asset] and                     // price asset
        (JsPath \ "S").formatNullable[OrderType] and                 // side: BUY or SELL
        (JsPath \ "T").formatNullable[Boolean](isMarketFormat) and   // type: MARKET or LIMIT
        (JsPath \ "p").formatNullable[Long] and                      // price
        (JsPath \ "a").formatNullable[Long] and                      // amount
        (JsPath \ "f").formatNullable[Long] and                      // fee
        (JsPath \ "F").formatNullable[Asset] and                     // fee asset
        (JsPath \ "s").formatNullable[String](orderStatusFormat) and // status: ACCEPTED or FILLED or PARTIALLY_FILLED or CANCELLED
        (JsPath \ "q").formatNullable[Long] and                      // filled amount
        (JsPath \ "Q").formatNullable[Long] and                      // filled fee
        (JsPath \ "r").formatNullable[Long]                          // average filled price among all trades
    )(WsOrder.apply, unlift(WsOrder.unapply))
}
