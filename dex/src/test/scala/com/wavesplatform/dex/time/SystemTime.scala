package com.wavesplatform.dex.time

import org.scalatest.{BeforeAndAfterAll, Suite}

trait SystemTime extends BeforeAndAfterAll { _: Suite =>

  protected val time = new Time {
    override def correctedTime(): Long = System.currentTimeMillis()
    override def getTimestamp(): Long  = System.currentTimeMillis()
  }

  protected val zeroTime: Time = new Time {
    override def correctedTime(): Long = 0L
    override def getTimestamp(): Long = 0L
  }

  protected def ntpNow: Long = time.getTimestamp()

  override protected def afterAll(): Unit = {
    super.afterAll()
  }
}
