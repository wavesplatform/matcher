package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class SettingsResponse(matcherPublicKey: String, matcherVersion: String, priceAssets: List[String], orderVersions: List[Int], networkByte: Byte)
object SettingsResponse {
  implicit val format: Format[SettingsResponse] = Json.format
}
