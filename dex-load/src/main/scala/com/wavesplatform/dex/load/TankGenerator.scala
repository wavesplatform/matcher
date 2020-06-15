package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.nio.file.Files

import com.softwaremill.sttp.{MonadError => _}
import com.wavesplatform.dex.load.request._
import com.wavesplatform.dex.load.utils.{settings, _}
import com.wavesplatform.wavesj._
import com.wavesplatform.wavesj.matcher.Order.Type
import play.api.libs.json.{JsError, JsSuccess, JsValue}

import scala.jdk.CollectionConverters._
import scala.util.Random

// noinspection ScalaStyle
object TankGenerator {

//  private val executor: ExecutorService                          = Executors.newFixedThreadPool(20)
//  implicit private val blockingContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

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
          .map { case List(aa, pa) => if (aa >= pa) (aa, pa) else (pa, aa) }
          .map(Function.tupled(new AssetPair(_, _))))
      .take(count)
      .toList

    savePairs(randomAssetPairs)
  }

  private def distributeAssets(accounts: List[PrivateKeyAccount], assets: List[String]): Unit = {
    println(s"Distributing... ")
    val minimumNeededAssetBalance = settings.defaults.maxOrdersPerAccount * settings.defaults.minimalOrderPrice * 10000

    def massTransferFee(group: List[Transfer]) = settings.defaults.massTransferFee + (group.size + 1) * settings.defaults.massTransferMultiplier

    assets.foreach { asset =>
      println(s"\t -- $asset")
      accounts
        .map(account => new Transfer(account.getAddress, minimumNeededAssetBalance))
        .grouped(100)
        .foreach { group =>
          try services.node.send(Transactions.makeMassTransferTx(issuer, asset, group.asJava, massTransferFee(group), null))
          catch {
            case e: Exception => println(e)
          }
        }
    }

    println(s"\t -- WAVES")

    accounts
      .map(account => new Transfer(account.getAddress, settings.defaults.wavesPerAccount))
      .grouped(100)
      .foreach { group =>
        try services.node.send(Transactions.makeMassTransferTx(issuer, "WAVES", group.asJava, massTransferFee(group), null))
        catch {
          case e: Exception => println(e)
        }
      }
    println(s" Done")

    waitForHeightArise()
    waitForHeightArise()
  }

  private def mkOrders(accounts: List[PrivateKeyAccount], pairs: List[AssetPair], matching: Boolean): List[Request] = {
    print(s"Creating orders... ")
    val orders = (1 to settings.defaults.maxOrdersPerAccount).flatMap(
      _ =>
        accounts.map(mkOrder(
          _,
          if (math.random < 0.5 || !matching) Type.BUY else Type.SELL,
          settings.defaults.minimalOrderAmount + Random.nextInt(settings.defaults.minimalOrderAmount.toInt * 10),
          settings.defaults.minimalOrderPrice + Random.nextInt(settings.defaults.minimalOrderPrice.toInt * 10),
          pairs(Random.nextInt(pairs.length))
        )))

    println("Done")
    Random.shuffle(orders.map(Request(RequestType.POST, "/matcher/orderbook", RequestTag.PLACE, _))).toList
  }

  private def mkPairsAndDistribute(accounts: List[PrivateKeyAccount], pairsFile: Option[File], distributed: Boolean = false): List[AssetPair] = {
    val assets =
      if (Files.notExists(pairsFile.get.toPath)) mkAssets()
      else readAssetPairs(pairsFile).map(p => { s"${p.getAmountAsset}-${p.getPriceAsset}" }).mkString("-").split("-").toSet.toList

    val pairs =
      if (Files.notExists(pairsFile.get.toPath)) mkAssetPairs(assets)
      else readAssetPairs(pairsFile)

    if (!distributed)
      distributeAssets(accounts, assets)
    pairs
  }

  private def mkPlaces(accounts: List[PrivateKeyAccount],
                       requestsCount: Int,
                       pairsFile: Option[File],
                       distributed: Boolean = false): List[Request] = {
    println(s"Making requests for placing...")
    mkOrders(accounts, mkPairsAndDistribute(accounts, pairsFile, distributed), false).take(requestsCount)
  }

  private def mkMatching(accounts: List[PrivateKeyAccount],
                         requestsCount: Int,
                         pairsFile: Option[File],
                         distributed: Boolean = false): List[Request] = {
    println(s"Making requests for matching...")
    mkOrders(accounts, mkPairsAndDistribute(accounts, pairsFile, distributed), true).take(requestsCount)
  }

  private def mkCancels(accounts: List[PrivateKeyAccount], requestsCount: Int): List[Request] = {
    println("Making requests for cancelling...")

    val cancels = accounts
      .flatMap { a =>
        getOrderBook(a)
          .as[Array[JsValue]]
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
      }

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
        RequestTag.ORDER_BOOK_BY_PAIR_AND_KEY,
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

    val bp  = all.filter(_.tag.equals(RequestTag.ORDER_BOOK_BY_PAIR))
    val bpk = all.filter(_.tag.equals(RequestTag.ORDER_BOOK_BY_PAIR_AND_KEY))

    val byPair = List
      .fill(requestsCount / bp.length + 1)(bp)
      .flatten
      .take((requestsCount * obp).toInt)
    val byKey = List
      .fill(requestsCount / bpk.length + 1)(bpk)
      .flatten
      .take((requestsCount * obpk).toInt)

    Random.shuffle(byPair ++ byKey)
  }

  private def mkOrderStatuses(accounts: List[PrivateKeyAccount], requestsCount: Int): List[Request] = {
    print("Making requests for getting order status... ")

    val statuses = accounts
      .flatMap { a =>
        getOrderBook(a, false)
          .as[Array[JsValue]]
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
      }
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

    for (i <- 0 to requestsCount) {
      try {
        val acc  = accounts(new Random().nextInt(accounts.length - 1))
        val pair = pairs(new Random().nextInt(pairs.length - 1))
        services.matcher.placeOrder(
          acc,
          settings.matcherPublicKey,
          pair,
          Type.BUY,
          settings.defaults.minimalOrderPrice,
          settings.defaults.minimalOrderAmount,
          System.currentTimeMillis + 60 * 60 * 24 * 20 * 1000,
          settings.defaults.matcherFee,
          null,
          false
        )
        if (i % 50 == 0) println(s"$i orders of $requestsCount have been placed")
      } catch {
        case e: Exception => println(e.getMessage)
      }
    }
    println("Done")
  }

  private def mkAllTypes(accounts: List[PrivateKeyAccount], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests:")
    placeOrdersForCancel(accounts, (requestsCount * settings.distribution.placeOrder).toInt, pairsFile)
    Random.shuffle(
      mkOrderStatuses(accounts, (requestsCount * settings.distribution.orderStatus).toInt) ++
        mkMatching(accounts, (requestsCount * settings.distribution.placeOrder).toInt, pairsFile, true) ++
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
