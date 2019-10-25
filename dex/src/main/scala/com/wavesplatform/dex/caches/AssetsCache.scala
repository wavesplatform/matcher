package com.wavesplatform.dex.caches

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset

trait AssetsCache {
  def put(id: Asset, name: ByteStr, decimals: Int): Unit
  def contains(id: Asset): Boolean
  def nameOf(id: Asset): ByteStr
  def decimalsOf(id: Asset): Int
}
