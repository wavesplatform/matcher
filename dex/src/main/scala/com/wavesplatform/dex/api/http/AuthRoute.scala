package com.wavesplatform.dex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directive0
import com.wavesplatform.api.http.ApiRoute
import com.wavesplatform.crypto
import com.wavesplatform.dex.api.{MatcherResponse, SimpleErrorResponse}
import com.wavesplatform.dex.error.{ApiKeyIsNotProvided, ApiKeyIsNotValid}
import com.wavesplatform.http.{`X-Api-Key`, api_key}

trait AuthRoute { this: ApiRoute =>

  protected val apiKeyHash: Option[Array[Byte]]

  def withAuth(implicit trm: ToResponseMarshaller[MatcherResponse]): Directive0 = {
    apiKeyHash.fold[Directive0] { complete(SimpleErrorResponse(StatusCodes.InternalServerError, ApiKeyIsNotProvided)) } { hashFromSettings =>
      optionalHeaderValueByType[`X-Api-Key`](()).flatMap {
        case Some(key) if java.util.Arrays.equals(crypto secureHash key.value, hashFromSettings) => pass
        case _ =>
          optionalHeaderValueByType[api_key](()).flatMap {
            case Some(key) if java.util.Arrays.equals(crypto secureHash key.value, hashFromSettings) => pass
            case _                                                                                   => complete { SimpleErrorResponse(StatusCodes.Forbidden, ApiKeyIsNotValid) }
          }
      }
    }
  }
}
