package com.wavesplatform.dex.load

import java.io.{File, PrintWriter}
import java.net.URI
import java.nio.file.Files
import java.util.concurrent.{ExecutorService, Executors}

import com.softwaremill.sttp.{MonadError => _}
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
import com.wavesplatform.dex.domain.account.{PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.load.request._
import com.wavesplatform.dex.load.utils._
import im.mak.waves.transactions.MassTransferTransaction
import im.mak.waves.transactions.account.{PrivateKey => JPrivateKey, PublicKey => JPublicKey}
import im.mak.waves.transactions.common.{Amount, AssetId}
import im.mak.waves.transactions.exchange.{AssetPair, Order, OrderType}
import im.mak.waves.transactions.mass.Transfer
import org.apache.http.HttpResponse
import org.apache.http.client.methods.RequestBuilder
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import play.api.libs.json.{JsError, JsSuccess, JsValue}

import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.util.Random

object TankGenerator {

  private val threadCount: Int          = 5
  private val executor: ExecutorService = Executors.newFixedThreadPool(threadCount)

  implicit private val blockingContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

  private val matcherHttpUri: URI = new URI(settings.hosts.matcher)

  val cm = new PoolingHttpClientConnectionManager
  cm.setDefaultMaxPerRoute(threadCount)
  val httpClient: CloseableHttpClient = HttpClients.custom.setConnectionManager(cm).build

  private def mkAccounts(seedPrefix: String, count: Int): List[JPrivateKey] = {
    print(s"Generating $count accounts (prefix: $seedPrefix)... ")
    val accounts = (1 to count).map(i => JPrivateKey.fromSeed(s"$seedPrefix$i", 0)).toList
    println("Done")
    accounts
  }

  private def mkAssets(count: Int = settings.assets.count): List[String] = {
    println(s"Generating $count assets... ")

    val assets = (1 to count).map(_ => mkAsset()).toList
    val asset  = assets(new Random().nextInt(assets.length))

    do { waitForHeightArise() } while (node.getAssetBalance(issuer.address(), asset) <= 0)

    println("Assets have been successfully issued")
    assets.map(_.toString)
  }

  private def mkAssetPairs(assets: List[String], count: Int = settings.assets.pairsCount): List[AssetPair] = {
    println(s"Creating $count asset pairs... ")

    val randomAssetPairs = Random
      .shuffle(
        assets
          .combinations(2)
          .map { case List(aa, pa) => if (aa >= pa) (aa, pa) else (pa, aa) }
          .map(Function.tupled((a, p) => new AssetPair(AssetId.as(a), AssetId.as(p)))))
      .take(count)
      .toList

    savePairs(randomAssetPairs)
  }

  private def distributeAssets(accounts: List[JPrivateKey], assets: List[String]): Unit = {
    println(s"Distributing... ")
    val minimumNeededAssetBalance = settings.defaults.maxOrdersPerAccount * settings.defaults.minimalOrderPrice * 10000

    def massTransferFee(group: List[Transfer]): Long = settings.defaults.massTransferFee + (group.size + 1) * settings.defaults.massTransferMultiplier

    def mkMassTransfer(transfers: List[Transfer], asset: AssetId): MassTransferTransaction =
      MassTransferTransaction
        .builder(transfers.asJava)
        .assetId(asset)
        .fee(massTransferFee(transfers))
        .version(1)
        .getSignedWith(issuer)

    assets.foreach { asset =>
      println(s"\t -- $asset")
      accounts
        .map(account => new Transfer(account.address(), minimumNeededAssetBalance))
        .grouped(100)
        .foreach { group =>
          try node.broadcast(mkMassTransfer(group, AssetId.as(asset)))
          catch { case e: Exception => println(e) }
        }
    }

    println(s"\t -- WAVES")

    accounts
      .map(account => new Transfer(account.address(), settings.defaults.wavesPerAccount))
      .grouped(100)
      .foreach { group =>
        try node.broadcast(mkMassTransfer(group, AssetId.WAVES))
        catch { case e: Exception => println(e) }
      }
    println(s" Done")

    val asset   = AssetId.as(assets(new Random().nextInt(assets.length)))
    val account = accounts(new Random().nextInt(accounts.length))

    while (node.getAssetBalance(account.address(), asset) == 0) waitForHeightArise()
  }

  private def mkOrders(accounts: List[JPrivateKey], pairs: List[AssetPair], matching: Boolean): List[Request] = {
    print(s"Creating orders... ")
    val orders = (1 to settings.defaults.maxOrdersPerAccount).flatMap(
      _ =>
        accounts.map(
          mkOrder(
            _,
            if (math.random() < 0.5 || !matching) OrderType.BUY else OrderType.SELL,
            settings.defaults.minimalOrderAmount + Random.nextInt(settings.defaults.minimalOrderAmount.toInt * 10),
            settings.defaults.minimalOrderPrice + Random.nextInt(settings.defaults.minimalOrderPrice.toInt * 10),
            pairs(Random.nextInt(pairs.length))
          )
      )
    )

    println("Done")
    Random
      .shuffle(
        orders.map(o => Request(RequestType.POST, "/matcher/orderbook", RequestTag.PLACE, o.toJson))
      )
      .toList
  }

  private def mkPairsAndDistribute(accounts: List[JPrivateKey], pairsFile: Option[File], distributed: Boolean = false): List[AssetPair] = {
    val assets =
      if (Files.notExists(pairsFile.get.toPath)) mkAssets()
      else
        readAssetPairs(pairsFile)
          .map(p => s"${p.left().toString}-${p.right().toString}")
          .mkString("-")
          .split("-")
          .toSet
          .toList

    val pairs: List[AssetPair] = if (Files.notExists(pairsFile.get.toPath)) mkAssetPairs(assets) else readAssetPairs(pairsFile)
    if (!distributed) distributeAssets(accounts, assets)

    pairs
  }

  private def mkPlaces(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File], distributed: Boolean = false): List[Request] = {
    println(s"Making requests for placing...")
    mkOrders(accounts, mkPairsAndDistribute(accounts, pairsFile, distributed), false).take(requestsCount)
  }

  private def mkMatching(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File], distributed: Boolean = false): List[Request] = {
    println(s"Making requests for matching...")
    mkOrders(accounts, mkPairsAndDistribute(accounts, pairsFile, distributed), true).take(requestsCount)
  }

  private def mkCancels(accounts: List[JPrivateKey], requestsCount: Int): List[Request] = {
    println("Making requests for cancelling...")

    val cancels = accounts
      .flatMap { a =>
        getOrderBook(a)
          .as[Array[JsValue]]
          .map { o =>
            val id = (o \ "id").as[String]
            val aa = ((o \ "assetPair").as[JsValue] \ "amountAsset").validate[String] match {
              case JsSuccess(name, _) => name
              case _: JsError         => "WAVES"
            }
            val pa = ((o \ "assetPair").as[JsValue] \ "priceAsset").validate[String] match {
              case JsSuccess(name, _) => name
              case _: JsError         => "WAVES"
            }

            val unsignedRequest =
              HttpCancelOrder(
                sender = PublicKey(a.publicKey().bytes()),
                orderId = Some(id.getBytes()),
                timestamp = None,
                signature = Array.emptyByteArray
              )

            val signedRequest =
              unsignedRequest.copy(
                signature = crypto.sign(
                  account = PrivateKey(a.bytes()),
                  message = unsignedRequest.toSign
                )
              )

            Request(RequestType.POST,
                    s"/matcher/orderbook/$aa/$pa/cancel",
                    RequestTag.CANCEL,
                    HttpCancelOrder.format.writes(signedRequest).toString())
          }
      }

    cancels.take(requestsCount)
  }

  private def mkOrderHistory(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting order history...")

    val pairs = readAssetPairs(pairsFile)
    val ts    = System.currentTimeMillis
    val obpk  = settings.distribution.orderBookByPairAndKey
    val obp   = settings.distribution.orderBookByPair

    def mkGetOrderBookByPairAndKey(a: JPrivateKey, p: AssetPair) = {
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.left().toString}/${p.right().toString}/publicKey/${a.publicKey().toString}?activeOnly=false&closedOnly=false",
        RequestTag.ORDER_BOOK_BY_PAIR_AND_KEY,
        headers = Map("Signature" -> getSignatureByPrivateKeyAndTimestamp(a, ts), "Timestamp" -> ts.toString)
      )
    }

    def mkGetOrderBookByPair(p: AssetPair) = {
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.left().toString}/${p.right().toString}",
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

  private def mkOrderStatuses(accounts: List[JPrivateKey], requestsCount: Int): List[Request] = {
    print("Making requests for getting order status... ")

    val statuses = accounts
      .flatMap { a =>
        getOrderBook(a, activeOnly = false)
          .as[Array[JsValue]]
          .map { o =>
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
          }
      }
    println("Done")

    Random.shuffle(
      List
        .fill(requestsCount / statuses.length + 1)(statuses)
        .flatten
        .take(requestsCount))
  }

  private def mkBalances(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting reserved and tradable balances... ")

    val pairs = readAssetPairs(pairsFile)
    val ts    = System.currentTimeMillis

    def mkTradableBalance(a: JPrivateKey, p: AssetPair): Request = {
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.left().toString}/${p.right().toString}/tradableBalance/${a.address()}",
        RequestTag.TRADABLE_BALANCE,
        headers = Map("Signature" -> getSignatureByPrivateKeyAndTimestamp(a, ts), "Timestamp" -> ts.toString)
      )
    }

    val all = accounts.flatMap { a =>
      pairs.map(mkTradableBalance(a, _))
    }

    Random
      .shuffle(
        List
          .fill(requestsCount / all.length + 1)(all)
          .flatten)
      .take(requestsCount)
  }

  def placeOrder(order: Order): Future[HttpResponse] = Future {
    val res = httpClient
      .execute(
        RequestBuilder
          .post(matcherHttpUri.resolve(s"/matcher/orderbook"))
          .setEntity(new StringEntity(order.toJson, ContentType.APPLICATION_JSON))
          .build()
      )
    res.close()
    res
  }

  def placeOrdersForCancel(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): Unit = {
    println("Placing some orders to prepare cancel-order requests... ")

    val pairs = mkPairsAndDistribute(accounts, pairsFile)

    val futures = (0 to requestsCount).map { _ =>
      val account   = accounts(new Random().nextInt(accounts.length))
      val assetPair = pairs(new Random().nextInt(pairs.length))

      val order =
        Order
          .builder(
            OrderType.BUY,
            Amount.of(settings.defaults.minimalOrderAmount, assetPair.left()),
            Amount.of(settings.defaults.minimalOrderPrice, assetPair.right()),
            JPublicKey.as(settings.matcherPublicKey)
          )
          .expiration(System.currentTimeMillis + 60 * 60 * 24 * 20 * 1000)
          .version(3)
          .getSignedWith(account)

      placeOrder(order)
    }

    val requestsAwaitingTime = (requestsCount / threadCount).seconds
    print(s"Awaiting place orders requests, requests count = $requestsCount, treads count = $threadCount, waiting at most $requestsAwaitingTime... ")
    Await.result(Future.sequence(futures), requestsAwaitingTime)
    println("Done")
  }

  private def mkAllTypes(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests:")

    placeOrdersForCancel(accounts, (requestsCount * settings.distribution.placeOrder).toInt, pairsFile)
    Random.shuffle(
      mkOrderStatuses(accounts, (requestsCount * settings.distribution.orderStatus).toInt) ++
        mkMatching(accounts, (requestsCount * settings.distribution.placeOrder).toInt, pairsFile, distributed = true) ++
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
    try {
      val accounts = mkAccounts(seedPrefix, accountsNumber)
      val requests = requestsType match {
        case 1 => mkPlaces(accounts, requestsCount, pairsFile)
        case 2 => mkCancels(accounts, requestsCount)
        case 3 => mkMatching(accounts, requestsCount, pairsFile)
        case 4 => mkOrderHistory(accounts, requestsCount, pairsFile)
        case 5 => mkBalances(accounts, requestsCount, pairsFile)
        case 6 => mkAllTypes(accounts, requestsCount, pairsFile)
        case _ => println("Wrong number of task "); List.empty
      }
      svRequests(requests, outputFile)
    } finally executor.shutdownNow()
  }
}
