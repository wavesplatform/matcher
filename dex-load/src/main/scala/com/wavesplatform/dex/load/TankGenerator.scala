package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files

import com.softwaremill.sttp.{MonadError => _}
import com.wavesplatform.dex.load.request._
import com.wavesplatform.dex.load.utils.{settings, _}
import com.wavesplatform.wavesj.matcher.Order.Type
import com.wavesplatform.wavesj.{AssetPair, Base58, PrivateKeyAccount, Transactions, Transfer}
import play.api.libs.json.{JsError, JsSuccess, JsValue}

import scala.util.Random
import scala.collection.JavaConversions.seqAsJavaList

object TankGenerator {
  private def mkAccounts(seedPrefix: String, count: Int): List[PrivateKeyAccount] = {
    print(s"Generating $count accounts (prefix: $seedPrefix)... ")
    val accounts = (1 to count).map(i => PrivateKeyAccount.fromSeed(s"$seedPrefix$i", 0, settings.chainId.charAt(0).toByte)).toList
    println("Done")
    accounts
  }

  private def mkAssets(count: Int = settings.assets.count): List[String] = {
    println(s"Generating $count assets... ")
    val assets = (1 to count).map(_ => mkAsset()).toList
    waitForHeightArise()
    println("Assets have been successfully issued")
    assets
  }

  private def mkAssetPairs(assets: List[String], count: Int = settings.assets.pairsCount): List[AssetPair] = {
    println(s"Creating $count asset pairs... ")

    val randomAssetPairs = Random
      .shuffle(
        assets
          .combinations(2)
          .map { case List(aa, pa) => if (aa > pa) (aa, pa) else (pa, aa) }
          .map(Function.tupled(new AssetPair(_, _))))
      .take(count)
      .toList

    savePairs(randomAssetPairs)
  }

  private def distributeAssets(accounts: List[PrivateKeyAccount], assets: List[String]): Unit = {
    println(s"Distributing... ")
    val amountPerUser             = settings.assets.quantity / 4 / accounts.length
    val minimumNeededAssetBalance = settings.defaults.maxOrdersPerAccount * settings.defaults.minimalOrderAmount * 2

    def assetBalanceIsNotEnough(ac: PrivateKeyAccount, as: String): Boolean = {
      services.node.getBalance(ac.getAddress, as) < minimumNeededAssetBalance &&
      services.node.getBalance(issuer.getAddress, as) > amountPerUser
    }

    def massTransferFee(group: List[Transfer]) = settings.defaults.massTransferFee + (group.size + 1) * settings.defaults.massTransferMultiplier

    println(s"\t assets... ")
    assets.foreach(asset => {
      accounts
        .map(account => {
          new Transfer(account.getAddress, if (assetBalanceIsNotEnough(account, asset)) minimumNeededAssetBalance else 1)
        })
        .grouped(100)
        .foreach(group => {
          val tx = Transactions.makeMassTransferTx(issuer, asset, seqAsJavaList[Transfer](group), massTransferFee(group), null)
          println(s"\t\tSending mass-transfer tx: ${mkJson(tx)}")
          services.node.send(tx)
        })
    })
    println(s" Done \n\tWaves... ")

    accounts
      .map(account => {
        new Transfer(account.getAddress, settings.defaults.wavesPerAccount)
      })
      .grouped(100)
      .foreach(group => {
        val tx = Transactions.makeMassTransferTx(issuer, "WAVES", seqAsJavaList[Transfer](group), massTransferFee(group), null)
        println(s"\t\tSending mass-transfer tx: ${mkJson(tx)}")
        services.node.send(tx)
      })
    println(s" Done")
    waitForHeightArise()
  }

  private def mkOrders(accounts: List[PrivateKeyAccount], pairs: List[AssetPair], matching: Boolean = true): List[Request] = {
    print(s"Creating orders... ")
    val safeAmount = settings.assets.quantity / 2 / accounts.length / settings.defaults.maxOrdersPerAccount

    val orders = (1 to settings.defaults.maxOrdersPerAccount).flatMap(
      _ =>
        accounts.map(mkOrder(
          _,
          if (math.random < 0.5 || !matching) Type.BUY else Type.SELL,
          Random.nextInt(safeAmount.toInt),
          Random.nextInt(safeAmount.toInt),
          pairs(Random.nextInt(pairs.length))
        )))

    println("Done")
    Random.shuffle(orders.map(Request(RequestType.POST, "/matcher/orderbook", RequestTag.PLACE, _))).toList
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
        getOrderBook(a)
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
            Request(RequestType.POST,
                    s"/matcher/orderbook/$aa/$pa/cancel",
                    RequestTag.CANCEL,
                    Transactions.makeOrderCancel(a, new AssetPair(aa, pa), id))
          })
      })

    cancels.take(requestsCount)
  }

  private def mkOrderHistory(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting order history...")

    val pairs = readAssetPairs(pairsFile)
    val ts    = System.currentTimeMillis
    val obpk  = settings.distribution.orderBookByPairAndKey
    val obp   = settings.distribution.orderBookByPair

    def mkGetOrderBookByPairAndKey(a: PrivateKeyAccount, p: AssetPair) = {
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.getAmountAsset}/${p.getPriceAsset}/publicKey/${Base58.encode(a.getPublicKey)}?activeOnly=false&closedOnly=false",
        RequestTag.ORDERBOOK_BY_PAIR_AND_KEY,
        headers = Map("Signature" -> services.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
      )
    }

    def mkGetOrderBookByPair(p: AssetPair) = {
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.getAmountAsset}/${p.getPriceAsset}",
        RequestTag.ORDER_BOOK_BY_PAIR
      )
    }

    val all = pairs.map(p => mkGetOrderBookByPair(p)) ++ accounts
      .flatMap(a => {
        pairs.map(p => mkGetOrderBookByPairAndKey(a, p))
      })

    val reserved = List
      .fill(requestsCount / all.length + 1)(all.filter(_.tag.equals(RequestTag.ORDER_BOOK_BY_PAIR)))
      .flatten
    val tradable = List
      .fill(requestsCount / all.length + 1)(all.filter(_.tag.equals(RequestTag.ORDERBOOK_BY_PAIR_AND_KEY)))
      .flatten

    Random.shuffle(reserved ++ tradable).take((requestsCount * (obp + obpk)).toInt)
  }

  private def mkOrderStatuses(accounts: List[PrivateKeyAccount], requestsCount: Int): List[Request] = {
    print("Making requests for getting order status... ")

    val statuses = accounts
      .flatMap(a => {
        getOrderBook(a, false)
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
            Request(
              RequestType.GET,
              s"/matcher/orderbook/$aa/$pa/$id",
              RequestTag.ORDER_STATUS
            )
          })
      })
    println("Done")

    Random.shuffle(
      List
        .fill(requestsCount / statuses.length + 1)(statuses)
        .flatten
        .take(requestsCount))
  }

  private def mkBalances(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting reserved and tradable balances... ")

    val pairs = readAssetPairs(pairsFile)
    val ts    = System.currentTimeMillis
    val tb    = settings.distribution.tradableBalance

    def mkTradableBalance(a: PrivateKeyAccount, p: AssetPair) = {
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.getAmountAsset}/${p.getPriceAsset}/tradableBalance/${a.getAddress}",
        RequestTag.TRADABLE_BALANCE,
        headers = Map("Signature" -> services.matcher.getOrderHistorySignature(a, ts), "Timestamp" -> ts.toString)
      )
    }

    val all = accounts.flatMap(a => {
      pairs.map(p => { mkTradableBalance(a, p) })
    })

    Random
      .shuffle(
        List
          .fill(requestsCount / all.length + 1)(all)
          .flatten)
      .take(requestsCount)
  }

  def placeOrdersForCancel(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): Unit = {
    println("Placing some orders to prepare cancel-order requests... ")
    val pairs = mkPairsAndDistribute(accounts, pairsFile)

    for (_ <- 0 to requestsCount) {
      val o = services.matcher.placeOrder(
        accounts(new Random().nextInt(accounts.length - 1)),
        settings.matcherPublicKey,
        pairs(new Random().nextInt(pairs.length - 1)),
        Type.BUY,
        settings.defaults.minimalOrderPrice,
        settings.defaults.minimalOrderAmount,
        System.currentTimeMillis + 60 * 60 * 24 * 29,
        settings.defaults.matcherFee,
        null,
        false
      )
      println(s"\tPlacing order ${o.getId}: ${mkJson(o)}")
    }
    println("Done")
  }

  private def mkAllTypes(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests:")
    placeOrdersForCancel(accounts, (requestsCount * settings.distribution.placeOrder).toInt, pairsFile)
    Random.shuffle(
      mkOrderStatuses(accounts, (requestsCount * settings.distribution.orderStatus).toInt) ++
        mkMatching(accounts, (requestsCount * settings.distribution.placeOrder).toInt, pairsFile) ++
        mkBalances(accounts, (requestsCount * settings.distribution.tradableBalance).toInt, pairsFile) ++
        mkOrderHistory(accounts, requestsCount, pairsFile) ++
        mkCancels(accounts, (requestsCount * settings.distribution.placeOrder).toInt)
    )
  }

  private def svRequests(requests: List[Request], outputFile: File): Unit = {
    println("\nAll data has been generated. Now it will be saved...")

    val output = new PrintWriter(outputFile, "utf-8")

    try requests.foreach(_.save(output))
    finally output.close()

    println(s"Generated: ${requests.length}")

    for (v <- RequestType.values) println(s"\t\t$v: ${requests.count(_.httpType.equals(v))}")
    for (v <- RequestTag.values) println(s"\t\t$v: ${requests.count(_.tag.equals(v))}")

    println(s"Results have been saved to $outputFile")
  }

  def mkRequests(seedPrefix: String, pairsFile: Option[File], outputFile: File, requestsCount: Int, requestsType: Int, accountsNumber: Int): Unit = {
    val accounts = mkAccounts(seedPrefix, accountsNumber)
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
