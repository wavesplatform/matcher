package com.wavesplatform.dex.settings

import scala.concurrent.duration._

case class WaitingOffsetQueueSettings(
  maxWaitingTime: FiniteDuration,
  checkInterval: FiniteDuration
)
