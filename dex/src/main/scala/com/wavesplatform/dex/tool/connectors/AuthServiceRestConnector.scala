package com.wavesplatform.dex.tool.connectors

import com.wavesplatform.dex.auth.JwtUtils
import com.wavesplatform.dex.cli.ErrorOr
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.tool.connectors.AuthServiceRestConnector.AuthCredentials

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

case class AuthServiceRestConnector(target: String, chainId: Byte) extends RestConnector with JwtUtils {

  private def mkAuthTokenRequestParams(keyPair: KeyPair): Map[String, String] = {
    val jwtPayload = mkJwtSignedPayload(keyPair, networkByte = chainId)
    Map(
      "grant_type" -> "password",
      "username" -> jwtPayload.publicKey.base58,
      "password" -> s"${jwtPayload.firstTokenExpirationInSeconds}:${jwtPayload.signature}",
      "scope" -> jwtPayload.scope.head,
      "client_id" -> jwtPayload.clientId
    )
  }

  def getAuthCredentials(maybeSeed: Option[String]): ErrorOr[AuthCredentials] = {
    val seed = maybeSeed getOrElse s"minion${ThreadLocalRandom.current.nextInt}"
    val keyPair = KeyPair(crypto secureHash (seed getBytes StandardCharsets.UTF_8))
    val requestParams = mkAuthTokenRequestParams(keyPair)

    mkResponse(_.post(targetUri).body(requestParams)).map { j =>
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
