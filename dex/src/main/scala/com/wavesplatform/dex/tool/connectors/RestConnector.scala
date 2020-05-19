package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import com.wavesplatform.dex.tool.ErrorOr
import com.wavesplatform.dex.tool.connectors.RestConnector.{ErrorOrJsonResponse, RequestFunction}
import play.api.libs.json.{JsValue, Json}
import sttp.client._

import scala.concurrent.duration._
import scala.util.Try

trait RestConnector extends Connector {

  implicit protected val backend: SttpBackend[Identity, Nothing, NothingT] = HttpURLConnectionBackend()

  protected def mkResponse(request: RequestFunction): ErrorOrJsonResponse =
    for {
      errorOrResponse <- Try { request(basicRequest).send().body }.toEither.leftMap(ex => s"Cannot send request! $ex")
      response        <- errorOrResponse
    } yield Json.parse(response)

  def swaggerRequest: ErrorOrJsonResponse = mkResponse { _.get(uri"$target/api-docs/swagger.json") }

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
