package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.market.MatcherActor.AssetInfo
import com.wavesplatform.dex.settings.OrderRestrictionsSettings
import play.api.libs.json.{Json, OFormat}

case class ApiMarketDataWithMeta(amountAsset: Asset,
                                 amountAssetName: String,
                                 amountAssetInfo: Option[AssetInfo],
                                 priceAsset: Asset,
                                 priceAssetName: String,
                                 priceAssetInfo: Option[AssetInfo],
                                 created: Long,
                                 restrictions: Option[OrderRestrictionsSettings],
                                 matchingRules: ApiOrderBookInfo.MatchingRuleSettings)

object ApiMarketDataWithMeta {
  implicit val apiMarketDataFormat: OFormat[ApiMarketDataWithMeta] = Json.format[ApiMarketDataWithMeta]
}
