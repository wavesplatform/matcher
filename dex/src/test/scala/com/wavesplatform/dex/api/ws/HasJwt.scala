package com.wavesplatform.dex.api.ws

import java.security
import java.security.KeyPairGenerator

import com.wavesplatform.dex.api.ws.protocol.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.auth.JwtUtils
import play.api.libs.json.{JsObject, Json}

trait HasJwt extends JwtUtils {

  protected val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(1024)
    kpg.generateKeyPair()
  }

  protected def mkJwt(payload: JwtPayload): String = mkJwt(authServiceKeyPair, Json.toJsObject(payload))
  protected def mkJwt(payload: JsObject): String = mkJwt(authServiceKeyPair, payload)
}
