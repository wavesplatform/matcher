package com.wavesplatform.dex.grpc.integration.dto

import com.wavesplatform.transaction.assets.exchange.AssetPair

case class BriefAssetDescription(name: String, decimals: Int, hasScript: Boolean = false)

object BriefAssetDescription {

  val wavesDescription =
    BriefAssetDescription(
      name = AssetPair.WavesName,
      decimals = 8
    )

  val someWavesDescription = Option(wavesDescription)
}
