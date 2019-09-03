package com.wavesplatform.it.api

import com.wavesplatform.it.util.{GlobalTimer, TimerExt}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Success, Try}

trait CanWait[F[_]] {
  def wait(duration: FiniteDuration): F[Unit]
}

object CanWait {
  implicit val futureCanWait = new CanWait[Future] {
    override def wait(duration: FiniteDuration): Future[Unit] = GlobalTimer.instance.sleep(duration)
  }

  implicit val tryCanWait = new CanWait[Try] {
    override def wait(duration: FiniteDuration): Try[Unit] = {
      Thread.sleep(duration.toMillis)
      Success(())
    }
  }
}
