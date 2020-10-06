package com.wavesplatform.dex.auth

import java.security

import com.wavesplatform.dex.api.ws.protocol.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import pdi.jwt.{JwtAlgorithm, JwtJson}
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait JwtUtils {

  def mkJwt(authServiceKeyPair: security.KeyPair, payload: JwtPayload): String = mkJwt(authServiceKeyPair, Json.toJsObject(payload))

  def mkJwt(authServiceKeyPrivateKey: security.PrivateKey, payload: JsObject): String =
    JwtJson.encode(payload, authServiceKeyPrivateKey, JwtAlgorithm.RS256)

  def mkJwt(authServiceKeyPair: security.KeyPair, payload: JsObject): String =
    JwtJson.encode(payload, authServiceKeyPair.getPrivate, JwtAlgorithm.RS256)

  def mkJwtSignedPayload(
    clientKeyPair: KeyPair,
    networkByte: Byte = AddressScheme.current.chainId,
    lifetime: FiniteDuration = 1.hour
  ): JwtPayload =
    mkJwtNotSignedPayload(clientKeyPair, networkByte, lifetime).signed(clientKeyPair)

  def mkJwtNotSignedPayload(
    clientPublicKey: PublicKey,
    networkByte: Byte = AddressScheme.current.chainId,
    lifetime: FiniteDuration = 1.hour
  ): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + lifetime.toSeconds
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = clientPublicKey,
      networkByte = networkByte.toChar.toString,
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    )
  }

}

object JwtUtils extends JwtUtils
