package com.wavesplatform.dex.it.sttp

import com.softwaremill.sttp.{MonadError, Request, Response, SttpBackend}
import com.wavesplatform.dex.domain.utils.ScorexLogging

class LoggingSttpBackend[R[_], S](delegate: SttpBackend[R, S]) extends SttpBackend[R, S] with ScorexLogging {

  override def send[T](request: Request[T, S]): R[Response[T]] = {
    val logRequest = !request.uri.path.mkString("/").endsWith("debug/print")

    val prefix = s"[${request.tag("requestId").fold("unknown")(_.toString.take(8))}]"
    if (logRequest) log.info(s"$prefix Sending $request")

    responseMonad
      .map {
        responseMonad.handleError(delegate send request) {
          case e: Exception =>
            if (logRequest) log.error(s"$prefix Exception during request: ${e.getMessage}")
            responseMonad.error(e)
        }
      } { response =>
        if (logRequest)
          if (response.isSuccess) log.debug(s"$prefix Got response: $response")
          else log.warn(s"$prefix Got response: $response")
        response
      }
  }

  override def close(): Unit = delegate.close()
  override def responseMonad: MonadError[R] = delegate.responseMonad
}
