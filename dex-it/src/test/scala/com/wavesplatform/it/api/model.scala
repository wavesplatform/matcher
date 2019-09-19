package com.wavesplatform.it.api

import play.api.libs.json.{Format, Json}

case class MatchingRules(tickSize: String)
object MatchingRules {
  implicit val matchingRules: Format[MatchingRules] = Json.format
}

case class OrderRestrictions(minAmount: String,
                             maxAmount: String,
                             stepAmount: String,
                             minPrice: String,
                             maxPrice: String,
                             stepPrice: String)
object OrderRestrictions {
  implicit val orderRestrictions: Format[OrderRestrictions] = Json.format
}

case class OrderbookInfo(matchingRules: MatchingRules,
                         restrictions: Option[OrderRestrictions])
object OrderbookInfo {
  implicit val orderbookInfo: Format[OrderbookInfo] = Json.format
}

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

case class MarketDataInfo(matcherPublicKey: String, markets: Seq[MarketData])
object MarketDataInfo {
  implicit val marketDataInfoResponseFormat: Format[MarketDataInfo] = Json.format
}