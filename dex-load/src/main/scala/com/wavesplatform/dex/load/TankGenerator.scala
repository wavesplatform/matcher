package com.wavesplatform.dex.load

import akka.util.ByteString
import com.wavesplatform.dex.api.http.protocol.HttpCancelOrder
import com.wavesplatform.dex.domain.account.{AddressScheme, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.load.request._
import com.wavesplatform.dex.load.utils._
import im.mak.waves.transactions.account.{PrivateKey => JPrivateKey, PublicKey => JPublicKey}
import im.mak.waves.transactions.common.{Amount, AssetId}
import im.mak.waves.transactions.exchange.{AssetPair, Order, OrderType}
import im.mak.waves.transactions.mass.Transfer
import im.mak.waves.transactions.{MassTransferTransaction, TransferTransaction, WavesConfig}
import org.apache.http.HttpResponse
import org.apache.http.client.config.{CookieSpecs, RequestConfig}
import org.apache.http.client.methods.RequestBuilder
import org.apache.http.client.protocol.HttpClientContext
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import play.api.libs.json.{JsError, JsSuccess, JsValue}

import java.io.{File, PrintWriter}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.util.Random
import scala.Ordered._

object TankGenerator {

  private val threadCount: Int = sys.env.getOrElse("SBT_THREAD_NUMBER", "3").toInt
  private val executor: ExecutorService = Executors.newFixedThreadPool(threadCount)

  implicit private val blockingContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

  private val matcherHttpUri: URI = new URI(settings.hosts.matcher)

  val cm = new PoolingHttpClientConnectionManager
  cm.setMaxTotal(20)
  cm.setDefaultMaxPerRoute(threadCount)

  val httpClient: CloseableHttpClient = HttpClients.custom
    .setDefaultRequestConfig(
      RequestConfig.custom
        .setSocketTimeout(10000)
        .setConnectTimeout(10000)
        .setConnectionRequestTimeout(10000)
        .setCookieSpec(CookieSpecs.STANDARD)
        .build
    )
    .setConnectionManager(cm)
    .build

  private def mkAccounts(seedPrefix: String, count: Int): List[JPrivateKey] = {
    print(s"Generating $count accounts (prefix: $seedPrefix)... ")
    WavesConfig.chainId(AddressScheme.current.chainId)

    val accounts = (0 until count).map { i =>
      val seedBytes = s"$seedPrefix$i".getBytes(StandardCharsets.UTF_8)
      JPrivateKey.as(KeyPair(ByteStr(seedBytes)).privateKey.arr)
    }
    println("Done")
    accounts.toList
  }

  def waitForEmptyUtx(): Unit = {
    println("Waiting for empty UTX...")
    while (node.getUtxSize() > 0)
      waitForHeightArise()
  }

  private def mkAssets(count: Int = settings.assets.count): List[String] = {
    println(s"Generating $count assets... ")

    val assetsTxs = for (_ <- 1 to count) yield mkAsset()
    val assets = assetsTxs.map(_.assetId())

    val futures = assetsTxs.map { tx =>
      Future {
        node.broadcast(tx)
      }
    }

    val requestsAwaitingTime = (count / threadCount).seconds
    print(
      s"Awaiting creating assets, requests count = $count, treads count = $threadCount, waiting at most $requestsAwaitingTime... "
    )
    Await.result(Future.sequence(futures), requestsAwaitingTime)

    waitForEmptyUtx()

    println("Assets have been successfully issued")
    assets.map(_.toString).toList
  }

  private def mkAssetPairs(assets: List[String], count: Int = settings.assets.pairsCount): List[AssetPair] = {
    println(s"Creating $count asset pairs... ")

    val randomAssetPairs = Random
      .shuffle {
        assets
          .combinations(2)
          .map {
            case List(aa, pa) => if (IssuedAsset(pa.getBytes()).compatId < IssuedAsset(aa.getBytes()).compatId) (aa, pa) else (pa, aa)
            case _ => throw new RuntimeException("Can't create asset-pair")
          }
          .map(Function.tupled((a, p) => new AssetPair(AssetId.as(a), AssetId.as(p))))
      }
      .take(count)
      .toList

    savePairs(randomAssetPairs)
  }

  private def mkMassTransfer(transfers: List[Transfer], asset: AssetId, ts: Long): MassTransferTransaction =
    MassTransferTransaction
      .builder(transfers.asJava)
      .assetId(asset)
      .fee(settings.defaults.massTransferFee + (transfers.size + 1) * settings.defaults.massTransferMultiplier)
      .version(1)
      .timestamp(ts)
      .getSignedWith(issuer)

  private def distributeAssets(
    accounts: List[JPrivateKey],
    assets: List[String],
    minimumNeededAssetBalance: Long = settings.defaults.maxOrdersPerAccount * settings.defaults.minimalOrderPrice * 10000
  ): Unit = {
    println("Distributing... ")

    val now = System.currentTimeMillis()

    assets.foreach { asset =>
      println(s"\t -- $asset")
      val futures = accounts
        .map(account => new Transfer(account.address(), minimumNeededAssetBalance))
        .grouped(100)
        .zipWithIndex
        .map { case (group, index) =>
          Future {
            node.broadcast(mkMassTransfer(transfers = group, asset = AssetId.as(asset), ts = now + index))
          }
        }
      val requestsAwaitingTime = (futures.size / threadCount).seconds
      Await.result(Future.sequence(futures), requestsAwaitingTime)
    }

    println(s"\t -- WAVES")

    accounts
      .map(account => new Transfer(account.address(), settings.defaults.wavesPerAccount))
      .grouped(100)
      .zipWithIndex
      .foreach { case (group, index) =>
        try node.broadcast(mkMassTransfer(transfers = group, asset = AssetId.as("WAVES"), ts = now + index))
        catch { case e: Exception => println(e) }
      }
    println(s" Done")

    waitForEmptyUtx()
  }

  private def mkOrders(accounts: List[JPrivateKey], pairs: List[AssetPair], matching: Boolean): List[Request] = {
    print(s"Creating orders... ")
    val orders = (1 to settings.defaults.maxOrdersPerAccount).flatMap(_ =>
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

    println(s"Pairs: ${pairs.mkString("\n")}")
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
              case _: JsError => "WAVES"
            }
            val pa = ((o \ "assetPair").as[JsValue] \ "priceAsset").validate[String] match {
              case JsSuccess(name, _) => name
              case _: JsError => "WAVES"
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

            Request(
              RequestType.POST,
              s"/matcher/orderbook/$aa/$pa/cancel",
              RequestTag.CANCEL,
              HttpCancelOrder.format.writes(signedRequest).toString()
            )
          }
      }

    cancels.take(requestsCount)
  }

  private def mkOrderHistory(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting order history...")

    val pairs = readAssetPairs(pairsFile)
    val ts = System.currentTimeMillis
    val obpk = settings.distribution.orderBookByPairAndKey
    val obp = settings.distribution.orderBookByPair

    def mkGetOrderBookByPairAndKey(a: JPrivateKey, p: AssetPair) =
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.left().toString}/${p.right().toString}/publicKey/${a.publicKey().toString}?activeOnly=false&closedOnly=false",
        RequestTag.ORDER_BOOK_BY_PAIR_AND_KEY,
        headers = Map("Signature" -> getSignatureByPrivateKeyAndTimestamp(a, ts), "Timestamp" -> ts.toString)
      )

    def mkGetOrderBookByPair(p: AssetPair) =
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.left().toString}/${p.right().toString}",
        RequestTag.ORDER_BOOK_BY_PAIR
      )

    val all = pairs.map(p => mkGetOrderBookByPair(p)) ++ accounts
      .flatMap { a =>
        pairs.map(p => mkGetOrderBookByPairAndKey(a, p))
      }

    val bp = all.filter(_.tag.equals(RequestTag.ORDER_BOOK_BY_PAIR))
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
              case _: JsError => "WAVES"
            }
            val pa = ((o \ "assetPair").as[JsValue] \ "priceAsset").validate[String] match {
              case JsSuccess(name, _) => name
              case _: JsError => "WAVES"
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
        .take(requestsCount)
    )
  }

  private def mkBalances(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    println("Making requests for getting reserved and tradable balances... ")

    val pairs = readAssetPairs(pairsFile)
    val ts = System.currentTimeMillis

    def mkTradableBalance(a: JPrivateKey, p: AssetPair): Request =
      Request(
        RequestType.GET,
        s"/matcher/orderbook/${p.left().toString}/${p.right().toString}/tradableBalance/${a.address()}",
        RequestTag.TRADABLE_BALANCE,
        headers = Map("Signature" -> getSignatureByPrivateKeyAndTimestamp(a, ts), "Timestamp" -> ts.toString)
      )

    val all = accounts.flatMap { a =>
      pairs.map(mkTradableBalance(a, _))
    }

    Random
      .shuffle(
        List
          .fill(requestsCount / all.length + 1)(all)
          .flatten
      )
      .take(requestsCount)
  }

  private def mkMassTransfers(accounts: List[JPrivateKey], requestCount: Int): List[Request] = {

    val assets = mkAssets(10)
    val initialValue = 100000000000L
    val assetOwners = accounts.map(_ -> assets(new Random().nextInt(assets.length)))
    val allRecipients = assetOwners.map(_._1).toSet

    assetOwners.map { case (assetOwner, asset) =>
      try {
        node.broadcast(
          TransferTransaction
            .builder(assetOwner.address(), Amount.of(initialValue, AssetId.as(asset)))
            .timestamp(System.currentTimeMillis() + Random.nextLong(100000))
            .version(1)
            .getSignedWith(issuer)
        )
        node.broadcast(
          TransferTransaction
            .builder(assetOwner.address(), Amount.of(initialValue))
            .timestamp(System.currentTimeMillis() + Random.nextLong(100000))
            .version(1)
            .getSignedWith(issuer)
        )
      } catch { case e: Exception => println(e) }
    }

    waitForEmptyUtx()

    val now = System.currentTimeMillis()
    val massTransfers =
      for {
        i <- 0 to requestCount / 100
        (assetOwner, asset) <- assetOwners
      } yield mkMassTransfer(
        transfers = (allRecipients - assetOwner).map(recipient => Transfer.to(recipient.address(), 1000000L)).toList,
        asset = AssetId.as(asset),
        ts = now + i
      )

    Random
      .shuffle(massTransfers.toList).map { mt =>
        Request(
          RequestType.POST,
          s"/transactions/broadcast",
          RequestTag.MASS_TRANSFER,
          jsonBody = mt.toJson
        )
      }
  }

  def placeOrder(order: Order): Future[HttpResponse] = Future {
    val res = httpClient.execute(
      RequestBuilder
        .post(matcherHttpUri.resolve(s"/matcher/orderbook"))
        .setEntity(new StringEntity(order.toJson, ContentType.APPLICATION_JSON))
        .build(),
      HttpClientContext.create
    )
    res.close()
    if (res.getStatusLine.getStatusCode != 200)
      throw new RuntimeException(s"placing fail: ${order.toJson}")
    else println(s"Placing OK: ${order.toJson}")
    res
  }

  def placeOrdersForCancel(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): Unit = {
    println("Placing some orders to prepare cancel-order requests... ")

    val pairs = mkPairsAndDistribute(accounts, pairsFile)

    val futures = (0 to requestsCount).map { _ =>
      val account = accounts(new Random().nextInt(accounts.length))
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

      placeOrder(order).recover {
        case e: Throwable => println(e.getMessage); null
      }
    }

    val requestsAwaitingTime = (requestsCount / threadCount).seconds
    print(
      s"Awaiting place orders requests, requests count = $requestsCount, treads count = $threadCount, waiting at most $requestsAwaitingTime... "
    )
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

  private def mkAllTypesWithMassTransfers(accounts: List[JPrivateKey], requestsCount: Int, pairsFile: Option[File]): List[Request] = {
    val massTransfersAccounts = accounts.slice(accounts.size - 100, accounts.size)
    val step1And3RequestsCount = requestsCount / 100 * 16
    val step2MatcherRequestsCount = requestsCount - step1And3RequestsCount
    val massTransfersRequestsCount = step2MatcherRequestsCount / 15

    val requests = mkAllTypes(accounts.slice(0, accounts.size - massTransfersAccounts.size), requestsCount, pairsFile)
    val step1And3 = requests.slice(0, step1And3RequestsCount).toList
    val step2 = requests.slice(step1And3RequestsCount, step1And3RequestsCount + step2MatcherRequestsCount)
    val massTransfers = mkMassTransfers(massTransfersAccounts, massTransfersRequestsCount)

    Random.shuffle(step1And3 ++ step2 ++ massTransfers)
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

  def mkRequests(seedPrefix: String, pairsFile: Option[File], outputFile: File, requestsCount: Int, requestsType: Int, accountsNumber: Int): Unit =
    try {
      val accounts = mkAccounts(seedPrefix, accountsNumber)
      val requests = requestsType match {
        case 1 => mkPlaces(accounts, requestsCount, pairsFile)
        case 2 => mkCancels(accounts, requestsCount)
        case 3 => mkMatching(accounts, requestsCount, pairsFile)
        case 4 => mkOrderHistory(accounts, requestsCount, pairsFile)
        case 5 => mkBalances(accounts, requestsCount, pairsFile)
        case 6 => mkAllTypes(accounts, requestsCount, pairsFile)
        case 7 => mkAllTypesWithMassTransfers(accounts, requestsCount, pairsFile)
        case _ => println("Wrong number of task "); List.empty
      }
      svRequests(requests, outputFile)
    } finally executor.shutdownNow()

}
