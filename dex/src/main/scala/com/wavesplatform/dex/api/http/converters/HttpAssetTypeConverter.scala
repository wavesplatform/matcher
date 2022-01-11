package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.api.http.entities.HttpAssetType
import com.wavesplatform.dex.settings.AssetType

object HttpAssetTypeConverter {

  def toHttp(at: AssetType): HttpAssetType = at match {
    case AssetType.Amount => HttpAssetType.HttpAmount
    case AssetType.Price => HttpAssetType.HttpPrice
    case AssetType.Spending => HttpAssetType.HttpSpending
    case AssetType.Receiving => HttpAssetType.HttpReceiving
  }

}
