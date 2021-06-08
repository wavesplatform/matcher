package com.wavesplatform.dex.settings

import scala.concurrent.duration._

case class WaitingOffsetQueueSettings(
  maxWaitingTime: FiniteDuration,
  commandsPerSecond: Double = 2.5d,
  checkInterval: FiniteDuration = 5.seconds
)
