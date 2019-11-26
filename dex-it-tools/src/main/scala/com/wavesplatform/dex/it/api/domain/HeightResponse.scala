package com.wavesplatform.dex.it.api.domain

import play.api.libs.json.{Format, Json}

case class HeightResponse(height: Int)
object HeightResponse {
  implicit val format: Format[HeightResponse] = Json.format[HeightResponse]
}
