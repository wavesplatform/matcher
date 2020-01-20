package com.wavesplatform

import com.wavesplatform.it.api.MatcherCommand

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control.NonFatal

package object it {

  /**
    * @return The number of successful commands
    */
  def executeCommands(xs: Seq[MatcherCommand], ignoreErrors: Boolean = true, timeout: FiniteDuration = 3.minutes): Int = {
    Await.result(Future.sequence(xs.map(executeCommand(_))), timeout).sum
  }

  private def executeCommand(x: MatcherCommand, ignoreErrors: Boolean = true): Future[Int] = x match {
    case MatcherCommand.Place(api, order) => api.tryPlace(order).map(_.fold(_ => 0, _ => 1))
    case MatcherCommand.Cancel(api, owner, order) =>
      try api.tryCancel(owner, order).map(_.fold(_ => 0, _ => 1))
      catch {
        case NonFatal(e) =>
          if (ignoreErrors) Future.successful(0)
          else Future.failed(e)
      }
  }

  def choose[T](xs: IndexedSeq[T]): T = xs(Random.nextInt(xs.size))
}
