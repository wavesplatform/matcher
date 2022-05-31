package com.wavesplatform.dex.api.routes

import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.server.{Directive0, Directive1}
import com.wavesplatform.dex.api.http.entities.{MatcherResponse, SimpleErrorResponse}
import com.wavesplatform.dex.api.http.headers.{`X-Api-Key`, `X-User-Public-Key`, api_key}
import com.wavesplatform.dex.api.ws.routes.MatcherWebSocketRoute
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error.{ApiKeyIsNotProvided, ApiKeyIsNotValid, MatcherError, UserPublicKeyIsNotValid}

import java.security.MessageDigest

trait AuthRoute { this: ApiRoute =>

  protected val apiKeyHashes: List[Array[Byte]]

  def withAuth(implicit matcherResponseTrm: ToResponseMarshaller[MatcherResponse]): Directive0 = {
    pass
  }

  def withUserPublicKeyOpt(implicit matcherResponseTrm: ToResponseMarshaller[MatcherResponse]): Directive1[Option[PublicKey]] =
    optionalHeaderValueByType(`X-User-Public-Key`).flatMap {
      case None => provide(None)
      case Some(rawPublicKey) =>
        PublicKey.fromBase58String(rawPublicKey.value) match {
          case Left(e) => complete(SimpleErrorResponse(UserPublicKeyIsNotValid(e.reason)))
          case Right(x) => provide[Option[PublicKey]](Some(PublicKey(x)))
        }
    }

}
