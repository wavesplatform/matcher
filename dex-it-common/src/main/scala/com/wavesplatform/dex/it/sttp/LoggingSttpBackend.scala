package com.wavesplatform.dex.it.sttp

import com.softwaremill.sttp.{MonadError, Request, Response, SttpBackend}
import com.wavesplatform.dex.domain.utils.ScorexLogging

class LoggingSttpBackend[R[_], S](delegate: SttpBackend[R, S]) extends SttpBackend[R, S] with ScorexLogging {

  override def send[T](request: Request[T, S]): R[Response[T]] = {

    val prefix = s"[${request.tag("requestId").getOrElse("unknown")}]"
    log.info(s"$prefix Sending $request")

    responseMonad
      .map {
        responseMonad.handleError(delegate send request) {
          case e: Exception =>
            log.error(s"$prefix Exception during request: ${e.getMessage}")
            responseMonad.error(e)
        }
      } { response =>
        if (response.isSuccess) log.debug(s"$prefix Got response: $response")
        else log.warn(s"$prefix Got response: $response")
        response
      }
  }

  override def close(): Unit = delegate.close()
  override def responseMonad: MonadError[R] = delegate.responseMonad
}
