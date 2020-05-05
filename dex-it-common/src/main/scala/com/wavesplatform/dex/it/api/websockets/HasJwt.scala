package com.wavesplatform.dex.it.api.websockets

import java.security
import java.security.KeyPairGenerator
import java.util.Base64

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.auth.JwtUtils
import play.api.libs.json.Json

trait HasJwt extends JwtUtils {
  protected val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(2048)
    kpg.generateKeyPair()
  }

  protected def jwtPublicKeyConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.web-sockets.web-socket-handler.jwt-public-key = \"\"\"-----BEGIN PUBLIC KEY-----
       |${Base64.getEncoder.encodeToString(authServiceKeyPair.getPublic.getEncoded).grouped(64).mkString("\n")}
       |-----END PUBLIC KEY-----\"\"\"
       |""".stripMargin
  )

  protected def mkJwt(payload: JwtPayload): String = mkJwt(authServiceKeyPair, Json.toJsObject(payload))
}
