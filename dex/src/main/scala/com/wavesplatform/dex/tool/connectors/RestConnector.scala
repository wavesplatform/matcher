package com.wavesplatform.dex.tool.connectors

import cats.syntax.either._
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.tool.connectors.RestConnector.{ErrorOrJsonResponse, RequestFunction}
import play.api.libs.json.{JsValue, Json}
import sttp.client3._

import scala.util.Try

trait RestConnector extends Connector {

  implicit protected val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  protected lazy val targetUri = uri"$target"

  protected def mkResponse(request: RequestFunction): ErrorOrJsonResponse =
    for {
      errorOrResponse <- Try(backend.send(request(basicRequest)).body).toEither.leftMap(ex => s"Cannot send request! ${ex.getWithStackTrace}")
      response <- errorOrResponse
    } yield Json.parse(response)

  override def close(): Unit = backend.close()
}

object RestConnector {

  type ErrorOrJsonResponse = ErrorOr[JsValue]
  type RequestFunction = RequestT[Empty, Either[String, String], Any] => Request[Either[String, String], Any]

}
