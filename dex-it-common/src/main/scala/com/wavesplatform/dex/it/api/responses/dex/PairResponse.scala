package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class PairResponse(amountAsset: String, priceAsset: String)
object PairResponse {
  implicit val format: Format[PairResponse] = Json.format
}
