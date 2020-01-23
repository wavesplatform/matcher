package com.wavesplatform.dex.it.fp

import com.wavesplatform.dex.it.time.GlobalTimer
import com.wavesplatform.dex.it.time.GlobalTimer.TimerOpsImplicits

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Success, Try}

trait CanWait[F[_]] {
  def wait(duration: FiniteDuration): F[Unit]
}

object CanWait {

  implicit val future: CanWait[Future] = (duration: FiniteDuration) => GlobalTimer.instance.sleep(duration)

  implicit val tryCanWait: CanWait[Try] = (duration: FiniteDuration) => {
    Thread.sleep(duration.toMillis)
    Success(())
  }
}
