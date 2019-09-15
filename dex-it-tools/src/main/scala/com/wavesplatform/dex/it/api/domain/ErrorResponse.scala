package com.wavesplatform.dex.it.api.domain

import play.api.libs.json.{Format, Json}

case class ErrorResponse(error: Int, message: String)
object ErrorResponse {
  implicit val format: Format[ErrorResponse] = Json.format[ErrorResponse]
}
