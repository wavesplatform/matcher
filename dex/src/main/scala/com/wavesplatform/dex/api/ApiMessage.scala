package com.wavesplatform.dex.api

import play.api.libs.json.{Json, OFormat}

case class ApiMessage(message: String) extends AnyVal

object ApiMessage {
  implicit val apiMessageFormat: OFormat[ApiMessage] = Json.format
}
