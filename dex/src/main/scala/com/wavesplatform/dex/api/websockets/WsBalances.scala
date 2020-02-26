package com.wavesplatform.dex.api.websockets

import play.api.libs.json.{Format, Json}

case class WsBalances(tradable: Long, reserved: Long)

object WsBalances {
  implicit val format: Format[WsBalances] = Json.format
}
