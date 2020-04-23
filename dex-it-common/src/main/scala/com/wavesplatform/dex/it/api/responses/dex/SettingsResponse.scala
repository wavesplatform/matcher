package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class SettingsResponse(orderVersions: List[Int], priceAssets: List[String], networkByte: Byte, matcherPublicKey: String)
object SettingsResponse {
  implicit val format: Format[SettingsResponse] = Json.format
}
