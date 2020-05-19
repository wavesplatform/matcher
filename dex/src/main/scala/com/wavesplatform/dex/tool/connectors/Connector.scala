package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.tool.ErrorOr
import com.wavesplatform.dex.tool.connectors.RestConnector.RepeatRequestOptions

import scala.annotation.tailrec

trait Connector extends AutoCloseable {

  protected val target: String
  protected val repeatRequestOptions: RepeatRequestOptions

  final def repeatRequest[A](sendRequest: => ErrorOr[A])(test: ErrorOr[A] => Boolean): ErrorOr[A] = {

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
