package com.wavesplatform.dex.fp

trait MayBeEmpty[T] {
  def isEmpty(x: T): Boolean
  def empty: T
}
