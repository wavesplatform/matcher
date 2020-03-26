package com.wavesplatform.dex.api.http

import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Directive0
import com.wavesplatform.dex.api.{MatcherResponse, MatcherWebSocketRoute, SimpleErrorResponse}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error.{ApiKeyIsNotProvided, ApiKeyIsNotValid, MatcherError}

trait AuthRoute { this: ApiRoute =>

  protected val apiKeyHash: Option[Array[Byte]]

  def withAuth(implicit trm: ToResponseMarshaller[MatcherResponse]): Directive0 = {

    def correctResponse(statusCode: StatusCode, matcherError: MatcherError): ToResponseMarshallable = this match {
      case _: MatcherWebSocketRoute => matcherError.toWsHttpResponse(statusCode)
      case _                        => SimpleErrorResponse(statusCode, matcherError)
    }

    apiKeyHash.fold[Directive0] { complete(SimpleErrorResponse(StatusCodes.InternalServerError, ApiKeyIsNotProvided)) } { hashFromSettings =>
      optionalHeaderValueByType[`X-Api-Key`](()).flatMap {
        case Some(key) if java.util.Arrays.equals(crypto secureHash key.value, hashFromSettings) => pass
        case _ =>
          optionalHeaderValueByType[api_key](()).flatMap {
            case Some(key) if java.util.Arrays.equals(crypto secureHash key.value, hashFromSettings) => pass
            case _                                                                                   => complete { correctResponse(StatusCodes.Forbidden, ApiKeyIsNotValid) }
          }
      }
    }
  }
}
