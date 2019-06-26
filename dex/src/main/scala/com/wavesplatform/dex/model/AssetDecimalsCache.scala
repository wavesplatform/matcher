package com.wavesplatform.dex.model

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset

class AssetDecimalsCache(blockchain: Blockchain) {

  private val WavesDecimals      = 8
  private val assetDecimalsCache = new ConcurrentHashMap[Asset, Int](1000, 0.9f, 10)

  def get(asset: Asset): Int = {
    asset.fold { WavesDecimals } { issuedAsset =>
      Option(assetDecimalsCache.get(asset)) getOrElse {

        val assetDecimals =
          blockchain
            .assetDescription(issuedAsset)
            .map(_.decimals)
            .getOrElse { throw new Exception("Can not get asset decimals since asset not found!") }

        assetDecimalsCache.put(issuedAsset, assetDecimals)
        assetDecimals
      }
    }
  }
}
