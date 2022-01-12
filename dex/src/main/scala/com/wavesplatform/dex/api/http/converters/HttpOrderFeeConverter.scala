package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.api.http.entities.HttpOrderFeeMode
import com.wavesplatform.dex.api.http.entities.HttpOrderFeeMode._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.settings.OrderFeeSettings

object HttpOrderFeeConverter {

  def fromSettings(settings: OrderFeeSettings, matcherAccountFee: Long, allRates: Map[Asset, Double]): HttpOrderFeeMode = settings match {
    case x: OrderFeeSettings.DynamicSettings => FeeModeDynamic(x.maxBaseFee + matcherAccountFee, allRates)
    case OrderFeeSettings.FixedSettings(assetId, minFee) => FeeModeFixed(assetId, minFee)
    case OrderFeeSettings.PercentSettings(assetType, minFee) => FeeModePercent(HttpAssetTypeConverter.toHttp(assetType), minFee)
  }

}
