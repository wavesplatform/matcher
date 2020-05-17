package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import com.wavesplatform.dex.tool.connectors.RestConnector.RepeatRequestOptions._
import com.wavesplatform.dex.tool.connectors.RestConnector.{ErrorOr, ErrorOrJsonResponse, RepeatRequestOptions, RequestFunction}
import play.api.libs.json.{JsValue, Json}
import sttp.client.{Empty, HttpURLConnectionBackend, Identity, NothingT, Request, RequestT, SttpBackend, basicRequest}

import scala.annotation.tailrec
import scala.concurrent.duration.{FiniteDuration, _}

trait RestConnector extends Connector {

  implicit val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  override def close(): Unit = backend.close()

  protected def mkResponse(request: RequestFunction): ErrorOrJsonResponse = request(basicRequest).send().body.map(Json.parse)

  @tailrec
  final def repeatRequest[A](sendRequest: => ErrorOr[A])(test: ErrorOr[A] => Boolean)(implicit ro: RepeatRequestOptions = default): ErrorOr[A] = {
    if (ro.attemptsLeft == 0) "All attempts are out!".asLeft
    else {
      val response = sendRequest
      if (test(response)) response
      else {
        Thread.sleep(ro.delay.toMillis)
        repeatRequest(sendRequest)(test)(ro.decreaseAttempts)
      }
    }
  }
}

object RestConnector {

  type ErrorOr[A]          = Either[String, A]
  type ErrorOrJsonResponse = ErrorOr[JsValue]
  type RequestFunction     = RequestT[Empty, Either[String, String], Nothing] => Request[Either[String, String], Nothing]

  final case class RepeatRequestOptions(attemptsLeft: Int, delay: FiniteDuration) {
    def decreaseAttempts: RepeatRequestOptions = copy(attemptsLeft = attemptsLeft - 1)
  }

  object RepeatRequestOptions {
    val default: RepeatRequestOptions = RepeatRequestOptions(10, 1.second)
  }
}
