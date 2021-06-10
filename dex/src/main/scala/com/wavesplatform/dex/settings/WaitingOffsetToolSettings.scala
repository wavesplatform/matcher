package com.wavesplatform.dex.settings

import scala.concurrent.duration._

final case class WaitingOffsetToolSettings(
  maxWaitingTime: FiniteDuration,
  checkInterval: FiniteDuration
)
