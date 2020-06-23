package com.wavesplatform.dex.tool.connectors

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.dex.auth.JwtUtils
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.tool.connectors.AuthServiceRestConnector.AuthCredentials
import sttp.model.Uri.QuerySegment

case class AuthServiceRestConnector(target: String, chainId: Byte) extends RestConnector with JwtUtils {

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

    val seed          = maybeSeed getOrElse s"minion${ThreadLocalRandom.current.nextInt}"
    val keyPair       = KeyPair(crypto secureHash (seed getBytes StandardCharsets.UTF_8))
    val requestParams = mkAuthTokenRequestParams(keyPair)
    val uri           = targetUri.copy(querySegments = requestParams)

    mkResponse { _.post(uri) }.map { j =>
      AuthCredentials(
        keyPair = keyPair,
        token = (j \ "access_token").as[String],
        seed = seed
      )
    }
  }
}

object AuthServiceRestConnector {
  final case class AuthCredentials(keyPair: KeyPair, token: String, seed: String)
}
