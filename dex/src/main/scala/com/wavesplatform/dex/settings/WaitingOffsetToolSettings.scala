package com.wavesplatform.dex.settings

import scala.concurrent.duration._

final case class WaitingOffsetToolSettings(
  queueProcessingTimeout: FiniteDuration,
  maxWaitingTime: FiniteDuration,
  checkInterval: FiniteDuration
)
