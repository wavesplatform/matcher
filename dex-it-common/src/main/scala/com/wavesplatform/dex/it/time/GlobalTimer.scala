package com.wavesplatform.dex.it.time

import io.netty.util.{HashedWheelTimer, Timer}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

object GlobalTimer {

  val instance: Timer = new HashedWheelTimer()

  sys.addShutdownHook {
    instance.stop()
  }

  implicit class TimerOpsImplicits(val timer: Timer) extends AnyVal {
    def schedule[A](f: => Future[A], delay: FiniteDuration): Future[A] = {
      val p = Promise[A]()
      try {
        timer.newTimeout(_ => p.completeWith(f), delay.length, delay.unit)
      } catch {
        case NonFatal(e) => p.failure(e)
      }
      p.future
    }

    def sleep(term: FiniteDuration): Future[Unit] = schedule(Future.successful(()), term)
  }
}
