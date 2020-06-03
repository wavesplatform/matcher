package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files

import com.softwaremill.sttp.{MonadError => _}
import com.wavesplatform.dex.load.utils.{settings, _}
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.{AssetPair, Base58, PrivateKeyAccount, Transactions}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.util.Random

object TankGenerator {
  private def mkAccounts(seedPrefix: String, count: Int): List[PrivateKeyAccount] = {
    print(s"Generating $count accounts (prefix: $seedPrefix)... ")
    val accounts = (1 to count).map(i => PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, settings.networkByte)).toList
    println("Done")
    accounts
  }

  private def mkAssets(count: Int = 9): List[String] = {
    println(s"Generating $count assets... ")
    val assets = (1 to count).map(_ => mkAsset()).toList
    waitForHeightArise()
    println("Assets have been successfully issued")
    assets
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

  private def distributeAssets(accounts: List[PrivateKeyAccount], assets: List[String]): Unit = {
    println(s"Distributing assets to accounts... ")
    val amountPerUser = settings.assetQuantity / 4 / accounts.length
    accounts
      .flatMap { acc => //TODO: change to mass transfer transactions
        Transactions.makeTransferTx(settings.issuer, acc.getAddress, 10000000000L, "WAVES", 300000, "WAVES", "") :: assets.map { ass =>
          if (settings.node.getBalance(acc.getAddress, ass) < amountPerUser && settings.node.getBalance(settings.issuer.getAddress, ass) > amountPerUser)
            Transactions.makeTransferTx(settings.issuer, acc.getAddress, amountPerUser, ass, 300000, "WAVES", "")
          else Transactions.makeTransferTx(settings.issuer, acc.getAddress, 1, ass, 300000, "WAVES", "")
        }
      }
      .flatMap(tx => {
        println(s"\tSending transfer tx: ${mkJson(tx)}")
        settings.node.send(tx)
      })

    waitForHeightArise()
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
    Random.shuffle(orders.map(Request(Request.POST.toString, "/matcher/orderbook", Request.PLACE.toString, _))).toList
  }

  private def mkPairsAndDistribute(accounts: List[PrivateKeyAccount], pairsFile: Option[File]): List[AssetPair] = {
    val assets =
      if (Files.notExists(pairsFile.get.toPath)) mkAssets()
      else readAssetPairs(pairsFile).map(p => { s"${p.getAmountAsset}-${p.getPriceAsset}" }).mkString("-").split("-").toSet.toList

    val pairs =
      if (Files.notExists(pairsFile.get.toPath)) mkAssetPairs(assets)
      else readAssetPairs(pairsFile)

    distributeAssets(accounts, assets)
    pairs
  }

  private def mkPlaces(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File], matching: Boolean): List[Request] = {
    println(s"Making requests for ${if (matching) "matching" else "placing"} ...")

    mkOrders(accounts, mkPairsAndDistribute(accounts, pairsFile), matching).take(requestsCount)
  }

  private def mkPlaces(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] =
    mkPlaces(accounts, requestsCount, pairsFile: Option[File], matching = false)

  private def mkMatching(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] =
    mkPlaces(accounts, requestsCount, pairsFile: Option[File], matching = true)

  private def mkCancels(accounts: List[PrivateKeyAccount], requestsCount: Int): List[Request] = {
    println("Making requests for cancelling...")

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
            Request(Request.POST.toString,
                    s"/matcher/orderbook/$aa/$pa/cancel",
                    Request.CANCEL.toString,
                    Transactions.makeOrderCancel(a, new AssetPair(aa, pa), id))
          })
      })

    cancels.take(requestsCount)
  }

  private def mkOrderHistory(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting order history...")

    val pairs = readAssetPairs(pairsFile)
    val ts    = System.currentTimeMillis

    val all = accounts
      .flatMap(a => {
        Request(
          Request.GET.toString,
          s"/matcher/orderbook/${Base58.encode(a.getPublicKey)}",
          Request.ORDER_HISTORY_BY_ACC.toString,
          headers = Map("Signature" -> settings.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
        ) ::
          pairs.map(
          p =>
            Request(
              Request.GET.toString,
              s"/matcher/orderbook/${p.getAmountAsset}/${p.getPriceAsset}/publicKey/${Base58.encode(a.getPublicKey())}",
              Request.ORDER_HISTORY_BY_PAIR.toString,
              headers = Map("Signature" -> settings.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
          ))
      })

    val byPair = List
      .fill((100000 * settings.distribution.getOrElse(Request.ORDER_HISTORY_BY_PAIR.toString, 1.0)).toInt)(
        all.filter(_.tag.equals(Request.ORDER_HISTORY_BY_PAIR.toString)))
      .flatten
    val byAcc = List
      .fill((100000 * settings.distribution.getOrElse(Request.ORDER_HISTORY_BY_ACC.toString, 1.0)).toInt)(
        all.filter(_.tag.equals(Request.ORDER_HISTORY_BY_ACC.toString)))
      .flatten

    Random.shuffle(byPair ++ byAcc).take(requestsCount)
  }

  private def mkBalances(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting reserved and tradable balances... ")

    val pairs = readAssetPairs(pairsFile)
    val ts    = System.currentTimeMillis

    val all = accounts.flatMap(a => {
      Request(
        Request.GET.toString,
        s"/matcher/balance/reserved/${Base58.encode(a.getPublicKey())}",
        Request.RESERVED_BALANCE.toString,
        headers = Map("Signature" -> settings.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
      ) ::
        pairs.map(p => {
        Request(
          Request.GET.toString,
          s"/matcher/orderbook/${p.getAmountAsset}/${p.getPriceAsset}/tradableBalance/${a.getAddress}",
          Request.TRADABLE_BALANCE.toString,
          headers = Map("Signature" -> settings.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
        )
      })
    })

    val reserved = List
      .fill((100000 * settings.distribution.getOrElse(Request.RESERVED_BALANCE.toString, 1.0)).toInt)(
        all.filter(_.tag.equals(Request.RESERVED_BALANCE.toString)))
      .flatten
    val tradable = List
      .fill((100000 * settings.distribution.getOrElse(Request.TRADABLE_BALANCE.toString, 1.0)).toInt)(
        all.filter(_.tag.equals(Request.TRADABLE_BALANCE.toString)))
      .flatten

    Random.shuffle(reserved ++ tradable).take(requestsCount)
  }

  def placeOrdersForCancel(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): Unit = {
    println("Placing some orders to prepare cancel-order requests... ")
    val pairs = mkPairsAndDistribute(accounts, pairsFile)

    for (_ <- 0 to (requestsCount * settings.distribution.getOrElse(Request.CANCEL.toString, 1.0)).toInt) {
      val o = settings.matcher.placeOrder(
        accounts(new Random().nextInt(accounts.length - 1)),
        settings.matcherPublicKey,
        pairs(new Random().nextInt(pairs.length - 1)),
        Type.BUY,
        10000,
        10000,
        System.currentTimeMillis + 60 * 60 * 24 * 29,
        300000,
        null,
        false
      )
      println(s"\tPlacing order: ${mkJson(o)}")
    }
    println("Done")
  }

  private def mkAllTypes(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests:")
    placeOrdersForCancel(accounts, requestsCount, pairsFile)
    Random.shuffle(
      mkMatching(accounts, (requestsCount * settings.distribution.getOrElse(Request.PLACE.toString, 1.0)).toInt, pairsFile) ++
        mkBalances(accounts, requestsCount, pairsFile) ++
        mkOrderHistory(accounts, requestsCount, pairsFile) ++
        mkCancels(accounts, (requestsCount * settings.distribution.getOrElse(Request.PLACE.toString, 1.0)).toInt)
    )
  }

  private def svRequests(requests: List[Request], outputFile: File): Unit = {
    println("\nAll data has been generated. Now it will be saved...")

    val output = new PrintWriter(outputFile, "utf-8")

    try requests.foreach(_.save(output))
    finally output.close()

    println(s"Generated: ${requests.length}")
    println(s"\t${Request.POST}: ${requests.count(_.httpType.equals(Request.POST.toString))}")
    println(s"\t\t${Request.PLACE}: ${requests.count(_.tag.equals(Request.PLACE.toString))}")
    println(s"\t\t${Request.CANCEL}: ${requests.count(_.tag.equals(Request.CANCEL.toString))}")
    println(s"\t${Request.GET}: ${requests.count(_.httpType.equals(Request.GET.toString))}")
    println(s"\t\t${Request.ORDER_HISTORY_BY_ACC}: ${requests.count(_.tag.equals(Request.ORDER_HISTORY_BY_ACC.toString))}")
    println(s"\t\t${Request.ORDER_HISTORY_BY_PAIR}: ${requests.count(_.tag.equals(Request.ORDER_HISTORY_BY_PAIR.toString))}")
    println(s"\t\t${Request.RESERVED_BALANCE}: ${requests.count(_.tag.equals(Request.RESERVED_BALANCE.toString))}")
    println(s"\t\t${Request.TRADABLE_BALANCE}: ${requests.count(_.tag.equals(Request.TRADABLE_BALANCE.toString))}")
    println(s"Results have been saved to $outputFile")
  }

  def mkRequests(seedPrefix: String, pairsFile: Option[File], outputFile: File, requestsCount: Int = 20000, requestsType: Int = 3): Unit = {
    val accounts = mkAccounts(seedPrefix, requestsCount / 400 + 1)
    val requests = requestsType match {
      case 1 => mkPlaces(accounts, requestsCount, pairsFile)
      case 2 => mkCancels(accounts, requestsCount)
      case 3 => mkMatching(accounts, requestsCount, pairsFile)
      case 4 => mkOrderHistory(accounts, requestsCount, pairsFile)
      case 5 => mkBalances(accounts, requestsCount, pairsFile)
      case 6 => mkAllTypes(accounts, requestsCount, pairsFile)
      case _ =>
        println("Wrong number of task ")
        List.empty
    }

    svRequests(requests, outputFile)
  }
}
