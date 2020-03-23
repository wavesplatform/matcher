package com.wavesplatform.dex.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive0, Directive1}
import com.wavesplatform.dex.api.{MatcherResponse, SimpleErrorResponse}
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error.{ApiKeyIsNotProvided, ApiKeyIsNotValid, UserPublicKeyIsNotValid}

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

  def withUserPublicKeyOpt(implicit trm: ToResponseMarshaller[MatcherResponse]): Directive1[Option[PublicKey]] =
    optionalHeaderValueByType[`X-User-Public-Key`](()).flatMap {
      case None => provide(None)
      case Some(rawPublicKey) =>
        PublicKey.fromBase58String(rawPublicKey.value) match {
          case Left(_)  => complete(SimpleErrorResponse(StatusCodes.BadRequest, UserPublicKeyIsNotValid))
          case Right(x) => provide[Option[PublicKey]](Some(PublicKey(x)))
        }
    }
}
