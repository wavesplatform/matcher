package com.wavesplatform.dex.load

import java.io.File
import java.security
import java.security.KeyPairGenerator
import java.io.BufferedWriter

import com.wavesplatform.dex.api.websockets.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.domain.account.{AddressScheme, PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.wavesj.PrivateKeyAccount
import play.api.libs.json.Json
import com.wavesplatform.dex.auth.JwtUtils

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.util.Random
import scala.io.Source

object GatlingFeeder {

  private val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(2048)
    kpg.generateKeyPair()
  }

  private def mkJwtSignedPayload(a: PrivateKeyAccount): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + 24.hour._1
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = PublicKey(a.getPublicKey()),
      networkByte = AddressScheme.current.chainId.toString,
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    ).signed(PrivateKey(a.getPrivateKey()))
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
  def mkFile(f: File, c: Int = 1000, p: String = "pairs.txt", s: String = "loadtest-", obs: Int = 10): Unit = {
    val o = s"${f.getAbsolutePath}/data-${System.currentTimeMillis()}.csv"
    var l = new ListBuffer[String]()
    for (i <- 0 to c) {
      val account = PrivateKeyAccount.fromSeed(s"$s$i", 0, 'D'.toByte)
      l += s"""${account.getAddress()};${mkAusString(account)};${mkObsStrings(p, obs)}\n"""
    }
    scala.tools.nsc.io.File(o).appendAll(l.mkString(""))
    println(s"Results has been saved to $o")
  }
}
