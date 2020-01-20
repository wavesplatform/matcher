package com.wavesplatform.dex.error

import com.wavesplatform.dex.domain.asset.Asset

trait ErrorFormatterContext {
  def assetDecimals(asset: Asset): Int
}
