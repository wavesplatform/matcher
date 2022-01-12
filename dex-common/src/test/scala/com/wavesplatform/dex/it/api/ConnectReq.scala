package com.wavesplatform.dex.it.api

import play.api.libs.json.{Format, Json}

case class ConnectReq(host: String, port: Int)

object ConnectReq {
  implicit val connectFormat: Format[ConnectReq] = Json.format
}
