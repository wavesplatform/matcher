package com.wavesplatform.dex.api

import play.api.libs.json.{Format, Json}

case class ApiMessage(message: String) extends AnyVal
object ApiMessage {
  implicit val apiMessageFormat: Format[ApiMessage] = Json.format
}
