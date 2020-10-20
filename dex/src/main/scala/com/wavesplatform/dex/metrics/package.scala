package com.wavesplatform.dex

import kamon.metric.Timer

import scala.concurrent.{ExecutionContext, Future}

package object metrics {

  implicit final class TimerExt(private val timer: Timer) extends AnyVal {

    def measureWithFilter[T](f: => T)(filter: T => Boolean): T = {
      val startedTimer = timer.start()
      val result = f
      if (filter(result)) startedTimer.stop()
      result
    }

    def measure[T](f: => T): T = measureWithFilter(f)(_ => true)

    def measureFuture[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
      val startedTimer = timer.start()
      val future = f
      future.onComplete(_ => startedTimer.stop())
      future
    }

  }

}
