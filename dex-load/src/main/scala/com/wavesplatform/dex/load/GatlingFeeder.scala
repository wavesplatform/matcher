package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.security
import java.security.KeyFactory
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64

import com.wavesplatform.dex.api.ws.protocol.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.auth.JwtUtils
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.utils.EitherExt2
import play.api.libs.json.Json

import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

object GatlingFeeder {

  def authServiceKeyPair(rawPrivateKey: String): security.PrivateKey = {
    val privateKeyContent = rawPrivateKey
      .replace("-----BEGIN PRIVATE KEY-----", "")
      .replace("-----END PRIVATE KEY-----", "")
      .replaceAll("\\n", "")

    val kf = KeyFactory.getInstance("RSA")
    val ksPkcs8 = new PKCS8EncodedKeySpec(Base64.getDecoder.decode(privateKeyContent))
    val privateKey = kf.generatePrivate(ksPkcs8)

    privateKey
  }

  private def mkJwtSignedPayload(a: KeyPair): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + 24.hour.toSeconds
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = a.publicKey,
      networkByte = AddressScheme.current.chainId.toChar.toString,
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    ).signed(a.privateKey)
  }

  private def mkAusString(accountKeyPair: KeyPair, authKp: security.PrivateKey): String =
    s"""{"T":"aus","S":"${accountKeyPair.toAddress.toString}","t":"jwt","j":"${JwtUtils.mkJwt(
      authKp,
      Json.toJsObject(mkJwtSignedPayload(accountKeyPair))
    )}"}"""

  private def mkObsStrings(pairsFile: File, numberPerClient: Int): String = {
    val source = Source.fromFile(pairsFile)
    try {
      val pairs = Random.shuffle(source.getLines().toVector)
      require(numberPerClient <= pairs.size, "numberPerClient > available asset pairs in file")
      pairs.take(numberPerClient).map(x => s"""{"T":"obs","S":"$x","d":100}""").mkString(";")
    } finally source.close()
  }

  def mkFile(
    accountsNumber: Int,
    seedPrefix: String,
    authKp: security.PrivateKey,
    pairsFile: File,
    orderBookNumberPerAccount: Int,
    feederFile: File
  ): Unit = {
    val output = new PrintWriter(feederFile, "utf-8")
    try {
      output.print("a;m")
      (0 until orderBookNumberPerAccount).foreach { i =>
        output.print(s";o$i")
      }
      output.println()
      (0 until accountsNumber).foreach { i =>
        val kp = KeyPair.fromSeed(Base58.encode(s"$seedPrefix$i".getBytes)).explicitGet()
        output.println(s"""${kp.toAddress};${mkAusString(kp, authKp)};${mkObsStrings(pairsFile, orderBookNumberPerAccount)}""")
      }
    } finally output.close()
    println(s"Results have been saved to $feederFile")
  }

}
