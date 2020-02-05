package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset
import play.api.libs.json.{Format, Json}

case class ApiMarket(
    amountAssetName: String,
    amountAsset: Asset,
    amountAssetInfo: ApiMarket.Info,
    priceAssetName: String,
    priceAsset: Asset,
    priceAssetInfo: ApiMarket.Info,
    matchingRules: ApiMarket.MatchingRules,
    created: Long
)

object ApiMarket {
  case class Info(decimals: Byte)
  object Info {
    implicit val infoFormat: Format[Info] = Json.format
  }

  case class MatchingRules(tickSize: String)
  object MatchingRules {
    implicit val matchingRulesFormat: Format[MatchingRules] = Json.format
  }

  implicit val apiMarketFormat: Format[ApiMarket] = Json.format
}
