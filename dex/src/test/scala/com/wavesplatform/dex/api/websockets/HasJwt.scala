package com.wavesplatform.dex.api.websockets

import java.security
import java.security.KeyPairGenerator

import com.wavesplatform.dex.api.websockets.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import pdi.jwt.{JwtAlgorithm, JwtJson}
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.DurationInt

trait HasJwt {
  protected val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(1024)
    kpg.generateKeyPair()
  }

  protected def mkJwt(payload: JwtPayload): String = mkJwt(Json.toJsObject(payload))
  protected def mkJwt(payload: JsObject): String   = JwtJson.encode(payload, authServiceKeyPair.getPrivate, JwtAlgorithm.RS256)

  protected def mkJwtSignedPayload(clientKeyPair: KeyPair,
                                   networkByte: Byte = AddressScheme.current.chainId,
                                   expiration: FiniteDuration = 1.minute): JwtPayload =
    mkJwtPayload(clientKeyPair, networkByte, expiration).signed(clientKeyPair)

  protected def mkJwtPayload(clientKeyPair: KeyPair,
                             networkByte: Byte = AddressScheme.current.chainId,
                             expiration: FiniteDuration = 1.minute): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + expiration.toSeconds
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = clientKeyPair,
      networkByte = networkByte.toChar.toString,
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    )
  }
}
