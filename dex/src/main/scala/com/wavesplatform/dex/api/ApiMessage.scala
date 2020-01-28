package com.wavesplatform.dex.api

import play.api.libs.json.{Format, Json}

case class ApiMessage(message: String)
object ApiMessage {
  implicit val apiMessageFormat: Format[ApiMessage] = Json.format
}
