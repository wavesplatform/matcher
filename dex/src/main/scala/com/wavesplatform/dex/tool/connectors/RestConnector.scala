package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import cats.syntax.option._
import com.wavesplatform.dex.tool.ErrorOr
import com.wavesplatform.dex.tool.connectors.RestConnector.{ErrorOrJsonResponse, RepeatRequestOptions, RequestFunction}
import play.api.libs.json.{JsValue, Json}
import sttp.client._

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.Try

trait RestConnector extends Connector {

  protected val repeatRequestOptions: RepeatRequestOptions

  implicit protected val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  protected def mkResponse(request: RequestFunction): ErrorOrJsonResponse =
    for {
      errorOrResponse <- Try { request(basicRequest).send().body }.toEither.leftMap(ex => s"Cannot send request! $ex")
      response        <- errorOrResponse
    } yield Json.parse(response)

  def swaggerRequest: ErrorOrJsonResponse = mkResponse { _.get(uri"$target/api-docs/swagger.json") }

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

  def waitForSwaggerJson: ErrorOrJsonResponse = repeatRequest(swaggerRequest)(_.isRight)

  override def close(): Unit = backend.close()
}

object RestConnector {

  type ErrorOrJsonResponse = ErrorOr[JsValue]
  type RequestFunction     = RequestT[Empty, Either[String, String], Nothing] => Request[Either[String, String], Nothing]

  final case class RepeatRequestOptions(attemptsLeft: Int, delay: FiniteDuration) {
    def decreaseAttempts: RepeatRequestOptions = copy(attemptsLeft = attemptsLeft - 1)
    override def toString: String              = s"max attempts = $attemptsLeft, interval = $delay"
  }
}
