package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.actors.OrderBookDirectoryActor.AssetInfo
import com.wavesplatform.dex.api.http.entities.HttpAssetInfo

object HttpAssetInfoConverter {

  def fromAssetInfo(ai: AssetInfo): HttpAssetInfo = HttpAssetInfo(ai.decimals)

}
