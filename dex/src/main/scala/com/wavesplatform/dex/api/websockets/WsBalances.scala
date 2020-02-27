package com.wavesplatform.dex.api.websockets

import play.api.libs.json._

case class WsBalances(tradable: Long, reserved: Long)

object WsBalances {

  implicit val reads: Reads[WsBalances] = Reads {
    _.validate[Seq[Long]] flatMap {
      case Seq(tradable, reserved) => JsSuccess(WsBalances(tradable, reserved))
      case _                       => JsError(s"Can't read balances!")
    }
  }

  implicit val writes: Writes[WsBalances] = Writes[WsBalances](wsb => Json.arr(wsb.tradable, wsb.reserved))

  implicit val format: Format[WsBalances] = Format(reads, writes)
}
