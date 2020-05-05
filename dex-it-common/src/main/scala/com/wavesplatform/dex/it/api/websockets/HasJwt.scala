package com.wavesplatform.dex.it.api.websockets

import java.security
import java.security.KeyPairGenerator

import com.wavesplatform.dex.api.websockets.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.auth.JwtUtils
import play.api.libs.json.Json

trait HasJwt extends JwtUtils {
  protected val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(2048)
    kpg.generateKeyPair()
  }

  protected def mkJwt(payload: JwtPayload): String = mkJwt(authServiceKeyPair, Json.toJsObject(payload))
}
