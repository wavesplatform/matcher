package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}

import com.google.common.primitives.Longs
import com.typesafe.config.ConfigFactory
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.matcher.{CancelOrder, Order}
import com.wavesplatform.wavesj.{AssetPair, PrivateKeyAccount, Transactions}
import com.softwaremill.sttp.Uri.QueryFragment
import com.softwaremill.sttp.playJson._
import com.softwaremill.sttp.{SttpBackend, MonadError => _, _}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.crypto

import scala.collection.mutable
import scala.util.Random

object TankGenerator extends ScorexLogging {
  private def mkAccounts(seedPrefix: String, count: Int): List[PrivateKeyAccount] = {
    print(s"Generating $count accounts (prefix: $seedPrefix)... ")
    val accounts = (1 to count).map(i => PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, settings.networkByte)).toList
    println("Done")
    accounts
  }

  private def mkAssets(count: Int = 9): List[String] = {
    println(s"Generating $count assets... ")
    val assets = (1 to count).map(_ => mkAsset).toList
    waitForHeightArise()
    println("Assets have been successfully issued")
    assets
  }

  private def distributeAssets(accounts: List[PrivateKeyAccount], assets: List[String]): Unit = {
    println(s"Distributing assets to accounts... ")
    val amountPerUser = settings.assetQuantity / 2 / accounts.length
    accounts
      .flatMap { acc =>
        Transactions.makeTransferTx(settings.issuer, acc.getAddress, 10000000000L, "WAVES", 300000, "WAVES", "") :: assets.map { ass =>
          Transactions.makeTransferTx(settings.issuer, acc.getAddress, amountPerUser, ass, 300000, "WAVES", "")
        }
      }
      .flatMap(settings.node.send(_))

    waitForHeightArise()
    println("Done")
  }

  private def mkAssetPairs(assets: List[String], count: Int = 10): List[AssetPair] = {
    print(s"Creating $count asset pairs... ")

    val randomAssetPairs = Random
      .shuffle(assets)
      .combinations(2)
      .map { case List(aa, pa) => if (aa > pa) (aa, pa) else (pa, aa) }
      .map(Function.tupled(new AssetPair(_, _)))

    println("Done")
    randomAssetPairs.take(count).toList
  }

  private def mkOrders(accounts: List[PrivateKeyAccount], pairs: List[AssetPair], matching: Boolean = true): List[Order] = {
    print(s"Creating orders... ")
    val safeAmount = settings.assetQuantity / 2 / accounts.length / 400

    val orders = accounts.map(
      mkOrder(_,
              if (math.random < 0.5) Type.BUY else Type.SELL,
              Random.nextInt(safeAmount.toInt),
              Random.nextInt(safeAmount.toInt),
              pairs(Random.nextInt(pairs.length))))

    println("Done")
    Random.shuffle(orders)
  }

  private def mkPlaces(seedPrefix: String, requestsCount: Int): Unit = {
    print("Making requests for placing...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400)
    val assets   = mkAssets()
    val pairs    = mkAssetPairs(assets)
    val orders   = mkOrders(accounts, pairs, false)

    distributeAssets(accounts, assets)
    svRequests(orders)
  }

  private def timestampAndSignatureHeaders(account: PrivateKeyAccount, timestamp: Long = System.currentTimeMillis): Map[String, String] = Map(
    "Timestamp" -> timestamp.toString,
    "Signature" -> settings.matcher.getOrderHistorySignature(account, timestamp)
  )

  private def mkCancels(seedPrefix: String, requestsCount: Int): Unit = {
    println("Making requests for cancelling...")
    val accounts                              = mkAccounts(seedPrefix, requestsCount / 400)
    val cancels: mutable.HashSet[CancelOrder] = mutable.HashSet()
    implicit val backend                      = HttpURLConnectionBackend()

    accounts.foreach(a => {

      println(
        sttp
          .get(uri"${settings.matcherUrl}/matcher/orderbook/${Base58.encode(a.getPublicKey)}")
          .headers(timestampAndSignatureHeaders(a))
          .send()
          .body
          .right
          .get
      )

      settings.matcher
        .getOrders(a)
        .forEach(o => {
          cancels += Transactions.makeOrderCancel(a, o.getAssetPair, o.getId.getBase58String)
        })
    })

    svRequests(cancels = cancels.toList)
  }

  private def mkMatching(seedPrefix: String, requestsCount: Int): Unit = {
    println("Making requests for matching...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400)
    val assets   = mkAssets()
    val pairs    = mkAssetPairs(assets)
    val orders   = mkOrders(accounts, pairs)

    distributeAssets(accounts, assets)
    svRequests(orders)
  }

  private def mkOrderHistory(seedPrefix: String, requestsCount: Int): Unit = {
    println("Making requests for getting order history...")
  }

  private def mkAllTypes(): Unit = {
    println("mkAllTypes")
  }

  private def svRequests(orders: List[Order] = List.empty, cancels: List[CancelOrder] = List.empty): Unit = {
    println("All data has been generated. Now it will be saved...")
    val requestsFile = new File(s"requests-${System.currentTimeMillis}.txt")
    val output       = new PrintWriter(requestsFile, "utf-8")
    try {
      orders.foreach(o => {
        output.println(mkPost(o, "/matcher/orderbook", "PLACE"))
      })
    } finally output.close()
    println(s"Generated orders count: ${orders.length}")
    println(s"Generated cancels count: ${cancels.length}")
    println(s"Results have been saved to $requestsFile")
  }

  def mkRequests(seedPrefix: String, requestsType: Int = 2, requestsCount: Int = 20000): Unit = {
    requestsType match {
      case 1 => mkPlaces(seedPrefix, requestsCount)
      case 2 => mkCancels(seedPrefix, requestsCount)
      case 3 => mkMatching(seedPrefix, requestsCount)
      case 4 => mkOrderHistory(seedPrefix, requestsCount)
      case 5 => mkAllTypes()
      case _ =>
        println("Wrong number of task ")
    }
  }
}
