package com.wavesplatform.dex.api.ws.entities

import com.wavesplatform.dex.api.ws.doubleAsStringFormat
import com.wavesplatform.dex.domain.order.OrderType
import play.api.libs.json._

import scala.collection.IndexedSeq

case class WsLastTrade(price: Double, amount: Double, side: OrderType)

object WsLastTrade {

  private implicit val doubleFormat: Format[Double] = doubleAsStringFormat

  implicit val wsLastTradeFormat: Format[WsLastTrade] = Format(
    fjs = Reads {
      case JsArray(IndexedSeq(price, amount, orderType)) =>
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
