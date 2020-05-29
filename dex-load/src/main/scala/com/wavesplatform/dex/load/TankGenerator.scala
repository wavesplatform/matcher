package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}

import com.softwaremill.sttp.{MonadError => _}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.load.utils._
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.{AssetPair, Base58, PrivateKeyAccount, Transactions}
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
      .flatMap { acc => //TODO: change to mass transfer transactions
        Transactions.makeTransferTx(settings.issuer, acc.getAddress, 10000000000L, "WAVES", 300000, "WAVES", "") :: assets.map { ass =>
          Transactions.makeTransferTx(settings.issuer, acc.getAddress, amountPerUser, ass, 300000, "WAVES", "")
        }
      }
      .flatMap(settings.node.send(_))

    waitForHeightArise()
    println("Done")
  }

  private def mkAssetPairs(assets: List[String], count: Int = 10): List[AssetPair] = {
    println(s"Creating $count asset pairs... ")

    val randomAssetPairs = Random
      .shuffle(assets)
      .combinations(2)
      .map { case List(aa, pa) => if (aa > pa) (aa, pa) else (pa, aa) }
      .map(Function.tupled(new AssetPair(_, _)))
      .take(count)
      .toList

    savePairs(randomAssetPairs)
  }

  private def mkOrders(accounts: List[PrivateKeyAccount], pairs: List[AssetPair], matching: Boolean = true): List[Request] = {
    print(s"Creating orders... ")
    val safeAmount = settings.assetQuantity / 2 / accounts.length / 400

    val orders = (0 to 399).flatMap(
      _ =>
        accounts.map(mkOrder(
          _,
          if (math.random < 0.5 || !matching) Type.BUY else Type.SELL,
          Random.nextInt(safeAmount.toInt),
          Random.nextInt(safeAmount.toInt),
          pairs(Random.nextInt(pairs.length))
        )))

    println("Done")
    Random.shuffle(orders.map(Request("POST", "/matcher/orderbook", "PLACE", _))).toList
  }

  private def mkPlaces(seedPrefix: String, requestsCount: Int): Unit = {
    print("Making requests for placing...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400 + 1)
    val assets   = mkAssets()
    val pairs    = mkAssetPairs(assets)
    val orders   = mkOrders(accounts, pairs, false)

    distributeAssets(accounts, assets)
    svRequests(orders.take(requestsCount))
  }

  private def mkCancels(seedPrefix: String, requestsCount: Int): Unit = {
    println("Making requests for cancelling...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400 + 1)

    val cancels = accounts
      .flatMap(a => {
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
            Request("POST", s"/matcher/orderbook/$aa/$pa/cancel", "CANCEL", Transactions.makeOrderCancel(a, new AssetPair(aa, pa), id))
          })
      })

    svRequests(cancels.take(requestsCount))
  }

  private def mkMatching(seedPrefix: String, requestsCount: Int): Unit = {
    println("Making requests for matching...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400)
    val assets   = mkAssets()
    val pairs    = mkAssetPairs(assets)
    val orders   = mkOrders(accounts, pairs)

    distributeAssets(accounts, assets)
    svRequests(orders.take(requestsCount))
  }

  private def mkOrderHistory(seedPrefix: String, requestsCount: Int, pairsFile: File): Unit = {
    println("Making requests for getting order history...")
    val accounts = mkAccounts(seedPrefix, requestsCount / 400)
    val pairs    = readPairs(pairsFile)
    val ts       = System.currentTimeMillis

    val all = accounts
      .flatMap(a => {
        Request(
          "GET",
          s"/matcher/orderbook/${Base58.encode(a.getPublicKey())}",
          "ORDER_HISTORY_BY_ACC",
          headers = Map("Signature" -> settings.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
        ) ::
          pairs.map(
          p =>
            Request(
              "GET",
              s"/matcher/orderbook/${p.getAmountAsset}/${p.getPriceAsset}/publicKey/${Base58.encode(a.getPublicKey())}",
              "ORDER_HISTORY_BY_PAIR",
              headers = Map("Signature" -> settings.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
          ))
      })

    svRequests(List.fill(1000)(all).flatten.take(requestsCount)) //TODO: calculate needed count
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

    println(s"Generated places count: ${requests.filter(_.tag.equals("PLACE")).length}")
    println(s"Generated cancels count: ${requests.filter(_.tag.equals("CANCEL")).length}")
    println(s"Results have been saved to $requestsFile")
  }

  def mkRequests(seedPrefix: String, requestsType: Int = 2, requestsCount: Int = 20000, pairsFile: Option[File]): Unit = {
    requestsType match {
      case 1 => mkPlaces(seedPrefix, requestsCount)
      case 2 => mkCancels(seedPrefix, requestsCount)
      case 3 => mkMatching(seedPrefix, requestsCount)
      case 4 => mkOrderHistory(seedPrefix, requestsCount, pairsFile.get)
      case 5 => mkAllTypes()
      case _ =>
        println("Wrong number of task ")
    }
  }
}
