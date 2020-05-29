package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}

import com.google.common.net.HttpHeaders
import com.softwaremill.sttp.{HttpURLConnectionBackend, MonadError => _, _}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.wavesj.json.WavesJsonMapper
import com.wavesplatform.wavesj.matcher.Order
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.{ApiJson, AssetPair, PrivateKeyAccount, Transactions}

import scala.io.Source
import scala.util.Random

package object utils {
  implicit val settings = new LoadTestSettings(ConfigFactory.parseResources(scala.util.Properties.envOrElse("CONF", "devnet.conf")))
  implicit val backend  = HttpURLConnectionBackend()


  def mkOrderHistoryHeaders(account: PrivateKeyAccount, timestamp: Long = System.currentTimeMillis): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> settings.matcher.getOrderHistorySignature(account, timestamp)
  )

  def getOrderBook(account: PrivateKeyAccount): String = {
    sttp
      .get(uri"${settings.matcherUrl}/matcher/orderbook/${Base58.encode(account.getPublicKey)}")
      .headers(mkOrderHistoryHeaders(account))
      .send()
      .body
      .right
      .get
  }

  def waitForHeightArise(): Unit = {
    val toHeight = settings.node.getHeight + 1
    print(s"\tWaiting for the next ($toHeight) block... ")
    while (settings.node.getHeight < toHeight) Thread.sleep(5000)
    println("Done")
  }

  def mkAsset(): String = {
    val tx =
      Transactions.makeIssueTx(
        settings.issuer,
        settings.networkByte,
        Random.nextInt(10000000).toString,
        Random.nextInt(10000000).toString,
        settings.assetQuantity,
        8, //TODO: random from 2 to 16
        false,
        null,
        settings.issueFee
      )
    println(s"\tSending Issue TX: ${mkJson(tx)}")
    settings.node.send(tx)
    tx.getId.toString
  }

  def mkOrder(acc: PrivateKeyAccount, orderType: Type, amount: Long, price: Long, pair: AssetPair): Order = {
    Transactions.makeOrder(acc,
                           settings.matcherPublicKey,
                           orderType,
                           pair,
                           price,
                           amount,
                           System.currentTimeMillis + 60 * 60 * 24 * 20 * 1000,
                           300000)
  }

  def mkJson(obj: ApiJson): String = new WavesJsonMapper(settings.networkByte).writeValueAsString(obj)

  def readPairs(file: File): List[AssetPair] =
    Source.fromFile(file).getLines.map(l => { new AssetPair(l.split("-")(0), l.split("-")(1)) }).toList

  def savePairs(pairs: List[AssetPair]): List[AssetPair] = {
    val requestsFile = new File(s"pairs.txt")
    val output       = new PrintWriter(requestsFile, "utf-8")

    try pairs.foreach(p => output.println(s"${p.getAmountAsset}-${p.getPriceAsset}"))
    finally output.close()

    println(s"\tDone. Pairs have been saved to: $requestsFile")
    pairs
  }
}
