package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.security
import java.security.KeyPairGenerator

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

  private val authServiceKeyPair: security.KeyPair = {
    val kpg = KeyPairGenerator.getInstance("RSA")
    kpg.initialize(2048)
    kpg.generateKeyPair()
  }

  private def mkJwtSignedPayload(a: PrivateKeyAccount): JwtPayload = {
    val exp = System.currentTimeMillis() / 1000 + 24.hour._1
    JwtPayload(
      signature = ByteStr(Array.emptyByteArray),
      publicKey = PublicKey(a.getPublicKey),
      networkByte = AddressScheme.current.chainId.toString,
      clientId = "test",
      firstTokenExpirationInSeconds = exp,
      activeTokenExpirationInSeconds = exp,
      scope = List("general")
    ).signed(PrivateKey(a.getPrivateKey))
  }

  private def mkJwt(a: PrivateKeyAccount): String = JwtUtils.mkJwt(authServiceKeyPair, Json.toJsObject(mkJwtSignedPayload(a)))

  private def mkAusString(a: PrivateKeyAccount): String = {
    s"""{"T":"aus","S":"${a.getAddress}","t":"jwt","j":"${mkJwt(a)}"}"""
  }

  private def mkObsStrings(pairsFile: File, numberPerClient: Int): String = {
    val source = Source.fromFile(pairsFile)
    try {
      val pairs = Random.shuffle(source.getLines.toVector)
      require(numberPerClient < pairs.size, "numberPerClient > available asset pairs in file")
      pairs.take(numberPerClient).map(x => s"""{"T":"obs","S":"$x","d":100}""").mkString(";")
    } finally source.close()
  }

  def mkFile(accountsNumber: Int, seedPrefix: String, pairsFile: File, orderBookNumberPerAccount: Int, feederFile: File): Unit = {
    val o      = s"${feederFile.getAbsolutePath}/data-${System.currentTimeMillis()}.csv"
    val output = new PrintWriter(o, "UTF_8")
    try {
      val accountIdxs = Random.shuffle((0 until accountsNumber).toVector)
      accountIdxs.foreach { i =>
        val pk = PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, AddressScheme.current.chainId)
        output.println(s"""${pk.getAddress};${mkAusString(pk)};${mkObsStrings(pairsFile, orderBookNumberPerAccount)}""")
      }
    } finally output.close()
    println(s"Results have been saved to $o")
  }
}
