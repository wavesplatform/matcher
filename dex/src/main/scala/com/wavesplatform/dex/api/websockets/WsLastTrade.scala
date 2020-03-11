package com.wavesplatform.dex.api.websockets

import com.wavesplatform.dex.domain.order.OrderType
import play.api.libs.json._

case class WsLastTrade(price: Double, amount: Double, side: OrderType)
object WsLastTrade {
  private implicit val doubleFormat = doubleAsStringFormat

  implicit val wsLastTradeFormat: Format[WsLastTrade] = {
    implicit val orderTypeFormat: Format[OrderType] = Format(
      fjs = Reads {
        case JsString("BUY")  => JsSuccess(OrderType.BUY)
        case JsString("SELL") => JsSuccess(OrderType.SELL)
        case x                => JsError(JsPath, s"Can't read OrderType from ${x.getClass.getName}")
      },
      tjs = Writes {
        case OrderType.BUY  => JsString("BUY")
        case OrderType.SELL => JsString("SELL")
      }
    )

    Format(
      fjs = Reads {
        case JsArray(Seq(price, amount, orderType)) =>
          for {
            price  <- price.validate[Double]
            amount <- amount.validate[Double]
            side   <- orderType.validate[OrderType]
          } yield WsLastTrade(price, amount, side)
        case x => JsError(JsPath, s"Can't read WsLastTrade from ${x.getClass.getName}")
      },
      tjs = Writes { x =>
        Json.arr(x.price, x.amount, x.side)
      }
    )
  }
}
