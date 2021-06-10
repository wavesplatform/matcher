package com.wavesplatform.dex.settings

import scala.concurrent.duration._

case class WaitingOffsetToolSettings(
  maxWaitingTime: FiniteDuration,
  checkInterval: FiniteDuration
)
