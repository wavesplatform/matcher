package com.wavesplatform.dex.caches

private[caches] case class DropOldestFixedBuffer2(prev: Double, latest: Double) {
  val min: Double = math.min(prev, latest)
  def append(e: Double): DropOldestFixedBuffer2 = DropOldestFixedBuffer2(latest, e)
}

private[caches] object DropOldestFixedBuffer2 {
  def apply(init: Double): DropOldestFixedBuffer2 = DropOldestFixedBuffer2(init, init)
}
