package com.wavesplatform.dex.it.fp

import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class RepeatRequestOptions(delayBetweenRequests: FiniteDuration, maxAttempts: Int) {
  def decreaseAttempts: RepeatRequestOptions = copy(maxAttempts = maxAttempts - 1)
}

object RepeatRequestOptions {
  val default = RepeatRequestOptions(1.second, 60)
}
