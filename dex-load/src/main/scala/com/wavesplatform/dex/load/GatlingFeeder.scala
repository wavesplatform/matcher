package com.wavesplatform.dex.load

import java.security
import java.security.KeyPairGenerator

import com.wavesplatform.dex.api.websockets.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.domain.account.{PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.wavesj.PrivateKeyAccount
import play.api.libs.json.Json
import com.wavesplatform.dex.auth.JwtUtils

import scala.concurrent.duration._
import scala.util.Random
import scala.io.Source

object GatlingFeeder {

  private val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(2048)
    kpg.generateKeyPair()
  }

  private def mkJwtSignedPayload(acc: PrivateKeyAccount): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + 24.hour._1
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = PublicKey(acc.getPublicKey()),
      networkByte = "D",
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    ).signed(PrivateKey(acc.getPrivateKey()))
  }

  private def mkJwt(a: PrivateKeyAccount): String = JwtUtils.mkJwt(authServiceKeyPair, Json.toJsObject(mkJwtSignedPayload(a)))

  private def mkAusString(a: PrivateKeyAccount): String = {
    s"""{"T":"aus","S":"${a.getAddress()}","t":"jwt","j":"${mkJwt(a)}"}"""
  }

  private def mkObsStrings(p: String, c: Int): String = {
    val l = Random.shuffle(Source.fromFile(p).getLines.toList)
    (for (i <- 0 to c) yield s"""{"T":"obs","S":"${l(i)}","d":100}""").mkString(";")
  }

  /**
    *
    * @param f   -- output file name
    * @param c   -- count of accounts
    * @param p   -- filename with asset pairs
    * @param s   -- seed without nonce
    * @param obs -- count connections to order book stream
    */
  def mkFile(f: String, c: Int, p: String = "pairs.txt", s: String = "loadtest-", obs: Int = 10): Unit = {
    for (i <- 0 to c) {
      val account = PrivateKeyAccount.fromSeed(s"$s$i", 0, 'D'.toByte)
      val str = s"""${account.getAddress()};${mkAusString(account)};${mkObsStrings(p, obs)}\n"""

      scala.tools.nsc.io.File(f).appendAll(str)
    }
  }
}
