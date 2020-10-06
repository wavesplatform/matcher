package com.wavesplatform.dex.it.api.responses.node

import play.api.libs.json.{Format, Json}

case class HeightResponse(height: Int)

object HeightResponse {
  implicit val format: Format[HeightResponse] = Json.format[HeightResponse]
}
