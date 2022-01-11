package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.api.http.entities.HttpAssetType
import com.wavesplatform.dex.settings.AssetType

object HttpAssetTypeConverter {

  def toHttp(at: AssetType): HttpAssetType = at match {
    case AssetType.Amount => HttpAssetType.Amount
    case AssetType.Price => HttpAssetType.Price
    case AssetType.Spending => HttpAssetType.Spending
    case AssetType.Receiving => HttpAssetType.Receiving
  }

}
