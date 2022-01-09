package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.api.http.entities.HttpV1LevelAgg
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.LevelAgg

object HttpV1LevelAggConverter {

  def fromLevelAgg(la: LevelAgg, assetPair: AssetPair)(implicit efc: ErrorFormatterContext): HttpV1LevelAgg = {
    val amountAssetDecimals =
      efc.assetDecimals(assetPair.amountAsset).getOrElse(throw new RuntimeException(s"Can't get asset decimals for ${assetPair.amountAsset}"))
    val priceAssetDecimals =
      efc.assetDecimals(assetPair.priceAsset).getOrElse(throw new RuntimeException(s"Can't get asset decimals for ${assetPair.amountAsset}"))
    HttpV1LevelAgg(
      Denormalization.denormalizeAmountAndFee(la.amount, amountAssetDecimals).toDouble,
      Denormalization.denormalizePrice(la.price, amountAssetDecimals, priceAssetDecimals).toDouble
    )
  }

}
