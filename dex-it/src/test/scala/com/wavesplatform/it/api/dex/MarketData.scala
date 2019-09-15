package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class MarketData(amountAsset: String,
                      amountAssetName: String,
                      priceAsset: String,
                      priceAssetName: String,
                      created: Long,
                      amountAssetInfo: Option[AssetDecimalsInfo],
                      priceAssetInfo: Option[AssetDecimalsInfo])
object MarketData {
  implicit val format: Format[MarketData] = Json.format
}
