package com.wavesplatform.dex.api.routes

import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.{Directive0, Directive1}
import com.wavesplatform.dex.api.http.entities.{MatcherResponse, SimpleErrorResponse}
import com.wavesplatform.dex.api.http.headers.{`X-Api-Key`, `X-User-Public-Key`, api_key}
import com.wavesplatform.dex.api.ws.routes.MatcherWebSocketRoute
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error.{ApiKeyIsNotProvided, ApiKeyIsNotValid, MatcherError, UserPublicKeyIsNotValid}

trait AuthRoute { this: ApiRoute =>

  protected val apiKeyHash: Option[Array[Byte]]

  def withAuth(implicit matcherResponseTrm: ToResponseMarshaller[MatcherResponse]): Directive0 = {

    def correctResponse(statusCode: StatusCode, matcherError: MatcherError): ToResponseMarshallable = this match {
      case _: MatcherWebSocketRoute => matcherError.toWsHttpResponse(statusCode)
      case _ => SimpleErrorResponse(statusCode, matcherError)
    }
    apiKeyHash.fold[Directive0](complete(SimpleErrorResponse(StatusCodes.InternalServerError, ApiKeyIsNotProvided))) { hashFromSettings =>
      optionalHeaderValueByType(`X-Api-Key`).flatMap {
        case Some(key) if java.util.Arrays.equals(crypto secureHash key.value, hashFromSettings) => pass
        case _ =>
          optionalHeaderValueByType(api_key).flatMap {
            case Some(key) if java.util.Arrays.equals(crypto secureHash key.value, hashFromSettings) => pass
            case _ => complete(correctResponse(StatusCodes.Forbidden, ApiKeyIsNotValid))
          }
      }
    }
  }

  def withUserPublicKeyOpt(implicit matcherResponseTrm: ToResponseMarshaller[MatcherResponse]): Directive1[Option[PublicKey]] =
    optionalHeaderValueByType(`X-User-Public-Key`).flatMap {
      case None => provide(None)
      case Some(rawPublicKey) =>
        PublicKey.fromBase58String(rawPublicKey.value) match {
          case Left(e) => complete(SimpleErrorResponse(StatusCodes.BadRequest, UserPublicKeyIsNotValid(e.reason)))
          case Right(x) => provide[Option[PublicKey]](Some(PublicKey(x)))
        }
    }

}
