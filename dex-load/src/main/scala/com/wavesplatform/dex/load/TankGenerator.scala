package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}

import com.softwaremill.sttp.{MonadError => _}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.{ApiJson, AssetPair, PrivateKeyAccount, Transactions}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

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

  private def mkOrders(accounts: List[PrivateKeyAccount], pairs: List[AssetPair], matching: Boolean = true): List[Request] = {
    print(s"Creating orders... ")
    val safeAmount = settings.assetQuantity / 2 / accounts.length / 400

    val orders = accounts.map(
      mkOrder(_,
              if (math.random < 0.5) Type.BUY else Type.SELL,
              Random.nextInt(safeAmount.toInt),
              Random.nextInt(safeAmount.toInt),
              pairs(Random.nextInt(pairs.length))))

    println("Done")
    Random.shuffle(orders.map(Request(_, "POST", "/matcher/orderbook", "PLACE")))
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

  private def mkCancels(seedPrefix: String, requestsCount: Int): Unit = {
    println("Making requests for cancelling...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400)

    val cancels = accounts.map(a => {
      Json
        .parse(getOrderBook(a))
        .as[List[JsValue]]
        .map(o => {
          val id = (o \ "id").as[String]
          val aa = ((o \ "assetPair").as[JsValue] \ "amountAsset").validate[String] match {
            case JsSuccess(name, _) => name
            case _: JsError         => "WAVES"
          }
          val pa = ((o \ "assetPair").as[JsValue] \ "priceAsset").validate[String] match {
            case JsSuccess(name, _) => name
            case _: JsError         => "WAVES"
          }
          Request(Transactions.makeOrderCancel(a, new AssetPair(aa, pa), id), "POST", s"/matcher/orderbook/$aa/$pa/cancel", "CANCEL")
        })
    })

    svRequests(cancels.flatten)
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

  private def svRequests(requests: List[Request]): Unit = {
    println("All data has been generated. Now it will be saved...")

    val requestsFile = new File(s"requests-${System.currentTimeMillis}.txt")
    val output       = new PrintWriter(requestsFile, "utf-8")

    try requests.foreach(_.save(output))
    finally output.close()

    println(s"Generated orders count: ${requests.filter(_.tag.equals("PLACE")).length}")
    println(s"Generated cancels count: ${requests.filter(_.tag.equals("CANCEL")).length}")
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
