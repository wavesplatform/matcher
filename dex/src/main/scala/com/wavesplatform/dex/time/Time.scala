package com.wavesplatform.dex.time

trait Time {
  def correctedTime(): Long
  def getTimestamp(): Long
}
