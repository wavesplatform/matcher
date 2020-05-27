package com.wavesplatform.dex.tool.connectors

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import cats.syntax.either._
import com.wavesplatform.dex.auth.JwtUtils
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.error.Implicits.ThrowableOps
import com.wavesplatform.dex.tool.connectors.AuthServiceRestConnector.AuthCredentials
import sttp.client._
import sttp.model.Uri.QuerySegment

import scala.util.Try

case class AuthServiceRestConnector(target: String, chainId: Byte) extends RestConnector with JwtUtils {

  def loginPageRequest: ErrorOr[Unit] = {
    val uri = uri"$hostPortUri/swagger-ui.html"
    for {
      errorOrResponse <- Try { basicRequest.get(uri).send().body }.toEither.leftMap(ex => s"Cannot load swagger UI! ${ex.getWithStackTrace}")
      _               <- errorOrResponse
    } yield ()
  }

  private def mkAuthTokenRequestParams(keyPair: KeyPair): List[QuerySegment] = {
    val jwtPayload = mkJwtSignedPayload(keyPair, networkByte = chainId)
    List(
      "grant_type" -> "password",
      "username"   -> jwtPayload.publicKey.base58,
      "password"   -> s"${jwtPayload.firstTokenExpirationInSeconds}:${jwtPayload.signature}",
      "scope"      -> jwtPayload.scope.head,
      "client_id"  -> jwtPayload.clientId
    ).map { case (k, v) => QuerySegment.KeyValue(k, v) }
  }

  def getAuthCredentials(maybeSeed: Option[String]): ErrorOr[AuthCredentials] = {

    val keyPair       = KeyPair(crypto secureHash (maybeSeed getOrElse s"minion${ThreadLocalRandom.current.nextInt}" getBytes StandardCharsets.UTF_8))
    val requestParams = mkAuthTokenRequestParams(keyPair)
    val uri           = targetUri.copy(querySegments = requestParams)

    mkResponse { _.post(uri) }.map { j =>
      AuthCredentials(
        keyPair = keyPair,
        token = (j \ "access_token").as[String]
      )
    }
  }
}

object AuthServiceRestConnector {
  final case class AuthCredentials(keyPair: KeyPair, token: String)
}
