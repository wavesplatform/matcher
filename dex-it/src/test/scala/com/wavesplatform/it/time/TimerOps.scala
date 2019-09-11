package com.wavesplatform.it.time

import io.netty.util.Timer

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

object TimerOps {
  implicit class Ops(val timer: Timer) extends AnyVal {
    def schedule[A](f: => Future[A], delay: FiniteDuration): Future[A] = {
      val p = Promise[A]
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
