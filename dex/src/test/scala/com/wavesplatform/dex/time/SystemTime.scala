package com.wavesplatform.dex.time

import org.scalatest.{BeforeAndAfterAll, Suite}

trait SystemTime extends BeforeAndAfterAll { _: Suite =>

  protected val time = Time.system
  protected val zeroTime = Time.zero

  protected def ntpNow: Long = time.getTimestamp()

  override protected def afterAll(): Unit =
    super.afterAll()

}
