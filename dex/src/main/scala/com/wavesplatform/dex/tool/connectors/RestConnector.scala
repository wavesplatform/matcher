package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import sttp.client.{HttpURLConnectionBackend, Identity, NothingT, SttpBackend}

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

trait RestConnector extends Connector {
  implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()
  override def close(): Unit                                     = backend.close()
}

object RestConnector {

  @tailrec
  def repeatRequest(attemptsLeft: Int, delay: FiniteDuration)(sendRequest: => Either[String, String],
                                                              test: Either[String, String] => Boolean): Either[String, String] = {
    if (attemptsLeft == 0) "All attempts are out!".asLeft
    else {
      val response = sendRequest
      if (test(response)) response
      else {
        Thread.sleep(delay.toMillis)
        repeatRequest(attemptsLeft - 1, delay)(sendRequest, test)
      }
    }
  }
}
