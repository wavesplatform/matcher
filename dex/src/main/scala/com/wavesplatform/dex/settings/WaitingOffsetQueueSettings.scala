package com.wavesplatform.dex.settings

import scala.concurrent.duration._

case class WaitingOffsetQueueSettings(
  maxWaitingTime: FiniteDuration,
  commandsPerSecond: Double,
  checkInterval: FiniteDuration
)
