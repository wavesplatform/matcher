package com.wavesplatform.dex

import kamon.metric.Timer
import monix.eval.Task

import scala.concurrent.{ExecutionContext, Future}

package object metrics {

  final implicit class TimerExt(private val timer: Timer) extends AnyVal {

    def measureWithFilter[T](f: => T)(filter: T => Boolean): T = {
      val startedTimer = timer.start()
      val result       = f
      if (filter(result)) startedTimer.stop()
      result
    }

    def measure[T](f: => T): T = measureWithFilter(f)(_ => true)

    def measureSuccessful[LeftT, RightT](f: => Either[LeftT, RightT]): Either[LeftT, RightT] = measureWithFilter(f)(_.isRight)
    def measureSuccessful[T](f: => Option[T]): Option[T]                                     = measureWithFilter(f)(_.isDefined)

    def measureTask[T](f: Task[T]): Task[T] = {
      Task
        .delay(timer.start())
        .flatMap { startedTimer =>
          f.guarantee(Task.delay { startedTimer.stop() })
        }
    }

    def measureFuture[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
      val startedTimer = timer.start()
      val future       = f
      future.onComplete(_ => startedTimer.stop())
      future
    }
  }
}
