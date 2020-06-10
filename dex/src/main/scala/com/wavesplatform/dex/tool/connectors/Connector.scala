package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.tool.connectors.Connector.RepeatRequestOptions

import scala.annotation.tailrec
import scala.concurrent.duration._

trait Connector extends AutoCloseable {

  protected val target: String

  implicit val repeatRequestOptions: RepeatRequestOptions = RepeatRequestOptions.default

  final def repeatRequest[A](sendRequest: => ErrorOr[A])(test: ErrorOr[A] => Boolean)(implicit repeatRequestOptions: RepeatRequestOptions =
                                                                                        repeatRequestOptions): ErrorOr[A] = {

    @tailrec
    def go(ro: RepeatRequestOptions, lastResponse: Option[ErrorOr[A]]): ErrorOr[A] = {
      if (ro.attemptsLeft == 0) s"All attempts are out! ${lastResponse.fold("")(lr => s"Last response: ${lr.fold(identity, _.toString)}")}".asLeft
      else {
        val response = sendRequest
        if (test(response)) response
        else {
          Thread.sleep(ro.delay.toMillis)
          go(ro.decreaseAttempts, response.some)
        }
      }
    }

    go(repeatRequestOptions, None)
  }
}

object Connector {

  final case class RepeatRequestOptions(attemptsLeft: Int, delay: FiniteDuration) {
    def decreaseAttempts: RepeatRequestOptions = copy(attemptsLeft = attemptsLeft - 1)
    override def toString: String              = s"max attempts = $attemptsLeft, interval = $delay"
  }

  object RepeatRequestOptions {
    val default: RepeatRequestOptions = RepeatRequestOptions(10, 1.second)
  }
}
