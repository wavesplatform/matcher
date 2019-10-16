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

// TODO rename entities named "MatcherSomething" to "Something" after entities from model of node will be relocated
case class MatcherOrderbookInfo(matchingRules: MatchingRules,
                         restrictions: Option[OrderRestrictions])
object MatcherOrderbookInfo {
  implicit val orderbookInfo: Format[MatcherOrderbookInfo] = Json.format
}

case class MatcherMarketData(amountAsset: String,
                      amountAssetName: String,
                      priceAsset: String,
                      priceAssetName: String,
                      created: Long,
                      amountAssetInfo: Option[AssetDecimalsInfo],
                      priceAssetInfo: Option[AssetDecimalsInfo],
                      matchingRules: MatchingRules,
                      restrictions: Option[OrderRestrictions])
object MatcherMarketData {
  implicit val marketData: Format[MatcherMarketData] = Json.format
}

case class MatcherMarketDataInfo(matcherPublicKey: String, markets: Seq[MatcherMarketData])
object MatcherMarketDataInfo {
  implicit val marketDataInfoResponseFormat: Format[MatcherMarketDataInfo] = Json.format
}