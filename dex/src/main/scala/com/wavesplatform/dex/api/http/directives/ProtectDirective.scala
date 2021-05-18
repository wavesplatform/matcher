package com.wavesplatform.dex.api.http.directives

import akka.http.scaladsl.server._
import com.wavesplatform.dex.api.http.entities.{InvalidJsonResponse, WavesNodeUnavailable}
import com.wavesplatform.dex.api.http.PlayJsonException
import com.wavesplatform.dex.api.routes.ApiRoute
import com.wavesplatform.dex.error._
import com.wavesplatform.dex.grpc.integration.exceptions.WavesNodeConnectionLostException

private[http] trait ProtectDirective { self: ApiRoute =>

  private def invalidJsonResponse(error: MatcherError): StandardRoute = complete(InvalidJsonResponse(error))

  protected def protect: Directive0 = handleExceptions(gRPCExceptionsHandler) & handleRejections(invalidJsonParsingRejectionsHandler)

  private val invalidJsonParsingRejectionsHandler =
    RejectionHandler
      .newBuilder()
      .handle {
        case ValidationRejection(_, Some(e: PlayJsonException)) => invalidJsonResponse(InvalidJson(e.errors.map(_._1.toString).toList))
        case _: UnsupportedRequestContentTypeRejection => invalidJsonResponse(UnsupportedContentType)
      }
      .result()

  private val gRPCExceptionsHandler: ExceptionHandler = ExceptionHandler {
    case ex: WavesNodeConnectionLostException =>
      log.error("Waves Node connection lost", ex)
      complete(WavesNodeUnavailable(WavesNodeConnectionBroken))
    case ex =>
      log.error("An unexpected error occurred", ex)
      complete(WavesNodeUnavailable(UnexpectedError))
  }

}
