package com.wavesplatform.dex.remote

import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.duration._

object Delay {

  // https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
  def fullJitter(base: FiniteDuration, attempt: Int, cap: FiniteDuration): FiniteDuration =
    if (attempt == 0) 0.nanos
    else {
      val max = (base.toNanos * math.pow(2.0, attempt)).toLong
      ThreadLocalRandom.current()
        .nextLong(
          0L,
          math.min(
            cap.toNanos,
            if (max > 0) max else cap.toNanos
          )
        )
        .nanos
    }

}
