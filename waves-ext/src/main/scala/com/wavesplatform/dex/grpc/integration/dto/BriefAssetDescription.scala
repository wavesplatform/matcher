package com.wavesplatform.dex.grpc.integration.dto

import com.wavesplatform.transaction.assets.exchange.AssetPair

case class BriefAssetDescription(name: String, decimals: Int, hasScript: Boolean, isNft: Boolean)

object BriefAssetDescription {

  val wavesDescription: BriefAssetDescription =
    BriefAssetDescription(
      name = AssetPair.WavesName,
      decimals = 8,
      hasScript = false,
      isNft = false
    )

  val someWavesDescription: Option[BriefAssetDescription] = Option(wavesDescription)
}
