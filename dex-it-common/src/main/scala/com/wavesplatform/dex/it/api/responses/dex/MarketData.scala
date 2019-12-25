package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class MarketData(amountAsset: String,
                      amountAssetName: String,
                      priceAsset: String,
                      priceAssetName: String,
                      created: Long,
                      amountAssetInfo: Option[AssetDecimalsInfo],
                      priceAssetInfo: Option[AssetDecimalsInfo],
                      matchingRules: MatchingRules,
                      restrictions: Option[OrderRestrictions])
object MarketData {
  implicit val marketData: Format[MarketData] = Json.format
}
