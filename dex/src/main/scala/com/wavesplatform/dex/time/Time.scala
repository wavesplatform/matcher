package com.wavesplatform.dex.time

trait Time {
  def correctedTime(): Long
  def getTimestamp(): Long
}

object Time {

  val system: Time = new Time {
    override def correctedTime(): Long = System.currentTimeMillis()
    override def getTimestamp(): Long = System.currentTimeMillis()
  }

  val zero: Time = new Time {
    override def correctedTime(): Long = 0L
    override def getTimestamp(): Long = 0L
  }

}
