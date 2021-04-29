package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files
import com.google.common.primitives.Longs
import sttp.client3._
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.wavesj.Node
import im.mak.waves.transactions.IssueTransaction
import im.mak.waves.transactions.account.{PrivateKey, PublicKey}
import im.mak.waves.transactions.common.{Amount, AssetId}
import im.mak.waves.transactions.exchange.{AssetPair, Order, OrderType}
import play.api.libs.json.{JsValue, Json}
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import sttp.client3.HttpURLConnectionBackend

import scala.io.Source
import scala.util.Random

package object utils {

  val settings: Settings =
    ConfigSource
      .fromConfig(ConfigFactory.parseResources(scala.util.Properties.envOrElse("CONF", "devnet.conf")).getConfig("waves.dex.load"))
      .load[Settings]
      .explicitGet()

  val node = new Node(settings.hosts.node)
  val networkByte = settings.chainId.charAt(0).toByte
  val issuer = PrivateKey.fromSeed(settings.richAccount, 0)
  implicit val backend = HttpURLConnectionBackend()

  def getSignatureByPrivateKeyAndTimestamp(pk: PrivateKey, timestamp: Long): String =
    crypto
      .sign(com.wavesplatform.dex.domain.account.PrivateKey.apply(pk.bytes()), pk.publicKey().bytes() ++ Longs.toByteArray(timestamp))
      .base58

  def mkOrderHistoryHeaders(account: PrivateKey, timestamp: Long = System.currentTimeMillis): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> getSignatureByPrivateKeyAndTimestamp(account, timestamp)
  )

  def getOrderBook(account: PrivateKey, activeOnly: Boolean = true): JsValue =
    Json
      .parse(
        basicRequest
          .get(uri"${settings.hosts.matcher}/matcher/orderbook/${account.publicKey().toString}?activeOnly=$activeOnly")
          .headers(mkOrderHistoryHeaders(account))
          .send(backend)
          .body
          .explicitGet()
      )

  def waitForHeightArise(): Unit = {
    val toHeight = node.getHeight + 1
    print(s"\tWaiting for the next ($toHeight) block... ")
    while (node.getHeight < toHeight) Thread.sleep(5000)
    println("Done")
  }

  def mkAsset(): IssueTransaction = {
    val tx =
      IssueTransaction
        .builder(Random.nextInt(10000000).toString, settings.assets.quantity, 8)
        .description(Random.nextInt(10000000).toString)
        .chainId(networkByte)
        .isReissuable(false)
        .script(null)
        .fee(settings.assets.issueFee)
        .version(2)
        .getSignedWith(issuer)

    println(s"\tCreated Issue TX: ${tx.toJson}")
    tx
  }

  def mkOrder(acc: PrivateKey, orderType: OrderType, amount: Long, price: Long, pair: AssetPair): Order =
    Order
      .builder(orderType, Amount.of(amount, pair.left()), Amount.of(price, pair.right()), PublicKey.as(settings.matcherPublicKey))
      .expiration(System.currentTimeMillis + 60 * 60 * 24 * 20 * 1000)
      .fee(settings.defaults.matcherFee)
      .version(3)
      .getSignedWith(acc)

  def readAssetPairs(file: Option[File]): List[AssetPair] =
    if (Files.exists(file.get.toPath)) {
      val source = Source.fromFile(file.get)
      val pairs =
        if (file.isEmpty) List.empty
        else
          source
            .getLines()
            .map { l =>
              val splitted = l.split("-")
              new AssetPair(AssetId.as(splitted(0)), AssetId.as(splitted(1)))
            }
            .toList
      source.close()
      pairs
    } else List.empty

  def savePairs(pairs: List[AssetPair]): List[AssetPair] = {
    val requestsFile = new File(settings.defaults.pairsFile)
    val output = new PrintWriter(requestsFile, "utf-8")

    try pairs.foreach(p => output.println(s"${p.left().toString}-${p.right().toString}"))
    finally output.close()

    println(s"\tDone. Pairs have been saved to: $requestsFile")
    pairs
  }

}
