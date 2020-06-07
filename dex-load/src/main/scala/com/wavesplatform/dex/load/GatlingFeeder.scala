package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.security
import java.security.KeyFactory
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64

import com.wavesplatform.dex.api.websockets.WsAddressSubscribe.JwtPayload
import com.wavesplatform.dex.auth.JwtUtils
import com.wavesplatform.dex.domain.account.{AddressScheme, PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.wavesj.PrivateKeyAccount
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

    val kf         = KeyFactory.getInstance("RSA")
    val ksPkcs8    = new PKCS8EncodedKeySpec(Base64.getDecoder.decode(privateKeyContent))
    val privateKey = kf.generatePrivate(ksPkcs8)

    privateKey
  }

  private def mkJwtSignedPayload(a: PrivateKeyAccount): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + 24.hour.toSeconds
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = PublicKey(a.getPublicKey),
      networkByte = AddressScheme.current.chainId.toChar.toString,
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    ).signed(PrivateKey(a.getPrivateKey))
  }

  private def mkJwt(accountPrivateKey: PrivateKeyAccount, authKp: security.PrivateKey): String =
    JwtUtils.mkJwt(authKp, Json.toJsObject(mkJwtSignedPayload(accountPrivateKey)))

  private def mkAusString(accountPrivateKey: PrivateKeyAccount, authKp: security.PrivateKey): String = {
    s"""{"T":"aus","S":"${accountPrivateKey.getAddress}","t":"jwt","j":"${mkJwt(accountPrivateKey, authKp)}"}"""
  }

  private def mkObsStrings(pairsFile: File, numberPerClient: Int): String = {
    val source = Source.fromFile(pairsFile)
    try {
      val pairs = Random.shuffle(source.getLines.toVector)
      require(numberPerClient <= pairs.size, "numberPerClient > available asset pairs in file")
      pairs.take(numberPerClient).map(x => s"""{"T":"obs","S":"$x","d":100}""").mkString(";")
    } finally source.close()
  }

  def mkFile(accountsNumber: Int,
             seedPrefix: String,
             authKp: security.PrivateKey,
             pairsFile: File,
             orderBookNumberPerAccount: Int,
             feederFile: File): Unit = {
    val output = new PrintWriter(feederFile, "utf-8")
    try {
      (0 until accountsNumber).foreach { i =>
        val pk = PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, AddressScheme.current.chainId)
        output.println(s"""${pk.getAddress};${mkAusString(pk, authKp)};${mkObsStrings(pairsFile, orderBookNumberPerAccount)}""")
      }
    } finally output.close()
    println(s"Results have been saved to $feederFile")
  }
}
