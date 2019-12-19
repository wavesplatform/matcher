package com.wavesplatform.dex.it.api.responses.node

import play.api.libs.json.{Format, Json}

case class AssetBalanceResponse(address: String, assetId: String, balance: Long)
object AssetBalanceResponse {
  implicit val assetBalanceFormat: Format[AssetBalanceResponse] = Json.format
}
