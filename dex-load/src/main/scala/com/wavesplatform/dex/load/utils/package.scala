package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files

import com.softwaremill.sttp.{HttpURLConnectionBackend, MonadError => _, _}
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.wavesj.json.WavesJsonMapper
import com.wavesplatform.wavesj.matcher.Order
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.{ApiJson, AssetPair, PrivateKeyAccount, Transactions}
import play.api.libs.json.{JsValue, Json}
import pureconfig._

import scala.io.Source
import scala.util.Random

package object utils {

  import pureconfig.generic.auto._

  val settings =
    ConfigSource
      .fromConfig(ConfigFactory.parseResources(scala.util.Properties.envOrElse("CONF", "devnet.conf")).getConfig("waves.dex.load"))
      .load[Settings]
      .explicitGet()

  val services         = new Services(settings)
  val networkByte      = settings.chainId.charAt(0).toByte
  val issuer           = PrivateKeyAccount.fromSeed(settings.richAccount, 0, networkByte)
  implicit val backend = HttpURLConnectionBackend()

  def mkOrderHistoryHeaders(account: PrivateKeyAccount, timestamp: Long = System.currentTimeMillis): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> services.matcher.getOrderHistorySignature(account, timestamp)
  )

  def getOrderBook(account: PrivateKeyAccount, activeOnly: Boolean = true): JsValue = {
    Json
      .parse(
        sttp
          .get(uri"${settings.hosts.matcher}/matcher/orderbook/${Base58.encode(account.getPublicKey)}?activeOnly=$activeOnly")
          .headers(mkOrderHistoryHeaders(account))
          .send()
          .body
          .explicitGet()
      )
  }

  def waitForHeightArise(): Unit = {
    val toHeight = services.node.getHeight + 1
    print(s"\tWaiting for the next ($toHeight) block... ")
    while (services.node.getHeight < toHeight) Thread.sleep(5000)
    println("Done")
  }

  def mkAsset(): String = {
    val tx =
      Transactions.makeIssueTx(
        issuer,
        networkByte,
        Random.nextInt(10000000).toString,
        Random.nextInt(10000000).toString,
        settings.assets.quantity,
        8, //TODO: random from 2 to 16
        false,
        null,
        settings.assets.issueFee
      )
    println(s"\tSending Issue TX: ${mkJson(tx)}")
    services.node.send(tx)
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
                           settings.defaults.matcherFee)
  }

  def mkJson(obj: ApiJson): String = new WavesJsonMapper(networkByte).writeValueAsString(obj)

  def readAssetPairs(file: Option[File]): List[AssetPair] = {
    if (Files.exists(file.get.toPath)) {
      val source = Source.fromFile(file.get)
      val pairs =
        if (file.isEmpty) List.empty
        else
          source.getLines
            .map(l => {
              val splitted = l.split("-")
              new AssetPair(splitted(0), splitted(1))
            })
            .toList
      source.close()
      pairs
    } else List.empty
  }

  def savePairs(pairs: List[AssetPair]): List[AssetPair] = {
    val requestsFile = new File(settings.defaults.pairsFile)
    val output       = new PrintWriter(requestsFile, "utf-8")

    try pairs.foreach(p => output.println(s"${p.getAmountAsset}-${p.getPriceAsset}"))
    finally output.close()

    println(s"\tDone. Pairs have been saved to: $requestsFile")
    pairs
  }
}
