package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class LevelResponse(amount: Long, price: Long)
object LevelResponse {
  implicit val format: Format[LevelResponse] = Json.format
}
