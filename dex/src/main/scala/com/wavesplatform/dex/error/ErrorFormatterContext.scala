package com.wavesplatform.dex.error

import com.wavesplatform.transaction.Asset

trait ErrorFormatterContext {
  def assetDecimals(asset: Asset): Option[Int]
}
