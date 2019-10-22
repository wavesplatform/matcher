package com.wavesplatform.it.sync

import java.sql.{Connection, DriverManager}
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.it.docker.DockerContainerLauncher
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{OrderStatus, OrderStatusResponse}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import io.getquill.{PostgresJdbcContext, SnakeCase}
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.{Failure, Try}

class OrderHistoryTestSuite extends MatcherSuiteBase {

  // scenario:
  //  1. create node network
  //  2. create postgres container (pgc)
  //  3. connect pgc to node network
  //  4. up pgc
  //  5. up node (during start node checks connection to postgres)
  //  6. send messages from node to pgc

  private val postgresImageName, postgresUser = "postgres"
  private val postgresContainerName           = s"pgc-${ThreadLocalRandom.current().nextInt(0, Int.MaxValue)}"
  private val postgresPassword                = "docker"
  private val postgresEnv                     = s"POSTGRES_PASSWORD=$postgresPassword"
  private val postgresContainerPort           = "5432"
  private val postgresContainerIp             = dockerClient.ipForNode(10)

  private val postgresContainerLauncher =
    new DockerContainerLauncher(
      imageName = postgresImageName,
      containerName = postgresContainerName,
      containerIp = postgresContainerIp,
      containerPort = postgresContainerPort,
      env = postgresEnv,
      networkName = dockerClient.network().name,
      imageTag = "10"
    )

  private val batchLingerMs: Int = OrderHistorySettings.defaultBatchLingerMs

  private def getPostgresContainerHostPort: String = postgresContainerLauncher.getHostPort.explicitGet()

  // TODO rewrite
  @annotation.tailrec
  private def retry[T](attemptsCount: Int, delayInMs: Int)(fn: => T): Try[T] = {
    Try { fn } match {
      case Failure(_) if attemptsCount > 1 => if (delayInMs != 0) Thread.sleep(delayInMs); retry(attemptsCount - 1, delayInMs)(fn)
      case Failure(ex)                     => throw ex
      case success                         => success
    }
  }

  private def createTables(postgresAddress: String): Unit = {
    val url                     = s"jdbc:postgresql://$postgresAddress/$postgresImageName"
    val orderHistoryDDLFileName = "/order-history/order-history-ddl.sql"

    def executeCreateTablesStatement(sqlConnection: Connection): Try[Unit] = Try {

      val fileStream            = getClass.getResourceAsStream(orderHistoryDDLFileName)
      val createTablesDDL       = Source.fromInputStream(fileStream).getLines.toSeq.mkString
      val createTablesStatement = sqlConnection.prepareStatement(createTablesDDL)

      createTablesStatement.executeUpdate()
      createTablesStatement.close()
    }

    retry(10, 2000) { DriverManager.getConnection(url, postgresUser, postgresPassword) } flatMap { sqlConnection =>
      executeCreateTablesStatement(sqlConnection).map(_ => sqlConnection.close())
    } get
  }

  private def getPostgresConnectionCfgString(serverName: String, port: String): String =
    s"""
       |postgres {
       |  server-name = $serverName
       |  port-number = $port
       |  user = $postgresUser
       |  password = $postgresPassword
       |  data-source-class-name = "org.postgresql.ds.PGSimpleDataSource"
       |}
    """.stripMargin

  private def getOrdersHistoryCfgString(batchLingerMs: Long): String =
    s"""
       |waves.dex {
       |  ${getPostgresConnectionCfgString(postgresContainerName, postgresContainerPort)}
       |  order-history {
       |    enabled = yes
       |    orders-batch-linger-ms = $batchLingerMs
       |    orders-batch-entries = 10000
       |    events-batch-linger-ms = $batchLingerMs
       |    events-batch-entries = 10000
       |  },
       |  allowed-order-versions = [1, 2, 3]
       |}
    """.stripMargin

  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(getOrdersHistoryCfgString(batchLingerMs))

  override protected def beforeAll(): Unit = {
    // DEX depends on Postgres, so it must start before
    postgresContainerLauncher.startContainer()
    createTables(s"localhost:$getPostgresContainerHostPort")

    super.beforeAll()

    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx)

    dex1Api.upsertRate(eth, 0.00567593)
    dex1Api.upsertRate(btc, 0.00009855)
    dex1Api.upsertRate(usd, 0.5)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    postgresContainerLauncher.stopAndRemoveContainer()
  }

  private lazy val ctx =
    new PostgresJdbcContext(
      SnakeCase,
      ConfigFactory
        .parseString(getPostgresConnectionCfgString("localhost", getPostgresContainerHostPort))
        .as[PostgresConnection]("postgres")
        .getConfig
    )

  import ctx._

  private def getOrdersCount: Long = ctx.run(querySchema[OrderRecord]("orders", _.id      -> "id").size)
  private def getEventsCount: Long = ctx.run(querySchema[EventRecord]("events", _.orderId -> "order_id").size)

  private case class OrderBriefInfo(id: String,
                                    tpe: Byte,
                                    senderPublicKey: String,
                                    side: Byte,
                                    amountAsset: String,
                                    priceAsset: String,
                                    feeAsset: String,
                                    amount: Double,
                                    price: Double,
                                    fee: Double)

  private case class EventBriefInfo(orderId: String,
                                    eventType: Byte,
                                    filled: Double,
                                    totalFilled: Double,
                                    feeFilled: Double,
                                    feeTotalFilled: Double,
                                    status: Byte)

  private def getOrderInfoById(orderId: Order.Id): Option[OrderBriefInfo] =
    ctx
      .run(
        querySchema[OrderBriefInfo](
          "orders",
          _.id              -> "id",
          _.tpe             -> "type",
          _.senderPublicKey -> "sender_public_key",
          _.side            -> "side",
          _.amountAsset     -> "amount_asset_id",
          _.priceAsset      -> "price_asset_id",
          _.feeAsset        -> "fee_asset_id",
          _.amount          -> "amount",
          _.price           -> "price",
          _.fee             -> "fee"
        ).filter(_.id == lift(orderId.toString))
      )
      .headOption

  private def getEventsInfoByOrderId(orderId: Order.Id): Set[EventBriefInfo] =
    ctx
      .run(
        querySchema[EventBriefInfo](
          "events",
          _.eventType      -> "event_type",
          _.filled         -> "filled",
          _.totalFilled    -> "total_filled",
          _.feeFilled      -> "fee_filled",
          _.feeTotalFilled -> "fee_total_filled",
          _.status         -> "status"
        ).filter(_.orderId == lift(orderId.toString))
      )
      .toSet

  implicit class DoubleOps(value: Double) {
    val wct, usd: Long      = Normalization.normalizeAmountAndFee(value, 2)
    val eth, btc: Long      = Normalization.normalizeAmountAndFee(value, 8)
    val wctUsdPrice: Long   = Normalization.normalizePrice(value, 2, 2)
    val wavesUsdPrice: Long = Normalization.normalizePrice(value, 8, 2)
  }

  def stringify(asset: Asset): String = AssetPair.assetIdStr(asset)

  "Order history should save all orders and events" in {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { i =>
        dex1Api.place(mkOrder(alice, wctUsdPair, BUY, 1.wct, 0.35.wctUsdPrice, 0.003.waves, ttl = 1.day + i.seconds))
        dex1Api.place(mkOrder(bob, wctUsdPair, SELL, 1.wct, 0.35.wctUsdPrice, 0.003.waves, ttl = 1.day + i.seconds))
      }

    retry(10, batchLingerMs) {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events: 1 big counter and 2 small submitted" in {

    def sellOrder: Order = mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.35.wctUsdPrice, matcherFee = 0.00000030.btc, matcherFeeAssetId = btc)
    val buyOrder         = mkOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.wctUsdPrice, matcherFee = 0.00001703.eth, matcherFeeAssetId = eth)

    dex1Api.place(buyOrder)

    val sellOrder1 = sellOrder
    dex1Api.place(sellOrder1)

    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1Api.waitForOrderStatus(sellOrder1, OrderStatus.Filled)

    val sellOrder2 = sellOrder
    dex1Api.place(sellOrder2)

    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1Api.waitForOrderStatus(sellOrder2, OrderStatus.Filled)

    dex1Api.cancel(alice, buyOrder)

    retry(10, batchLingerMs) {
      withClue("checking info for 2 small submitted orders\n") {

        Set(sellOrder1, sellOrder2).foreach { order =>
          getOrderInfoById(order.id()).get shouldBe
            OrderBriefInfo(order.idStr(),
                           limitOrderType,
                           bob.publicKey.toString,
                           sellSide,
                           stringify(wct),
                           stringify(usd),
                           stringify(btc),
                           100,
                           0.35,
                           0.00000030)

          getEventsInfoByOrderId(order.id()) shouldBe Set {
            EventBriefInfo(order.idStr(), eventTrade, 100, 100, 0.00000030, 0.00000030, statusFilled)
          }
        }
      }

      withClue("checking info for 1 big counter order\n") {
        getOrderInfoById(buyOrder.id()).get shouldBe
          OrderBriefInfo(buyOrder.idStr(),
                         limitOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         300,
                         0.35,
                         0.00001703)

        getEventsInfoByOrderId(buyOrder.id()) shouldBe
          Set(
            EventBriefInfo(buyOrder.idStr(), eventTrade, 100, 100, 0.00000567, 0.00000567, statusPartiallyFilled),
            EventBriefInfo(buyOrder.idStr(), eventTrade, 100, 200, 0.00000567, 0.00001134, statusPartiallyFilled),
            EventBriefInfo(buyOrder.idStr(), eventCancel, 0, 200, 0, 0.00001134, statusCancelled)
          )
      }
    }
  }

  "Order history should correctly save events with Waves as amount and fee" in {
    val buyOrder  = mkOrder(alice, wavesUsdPair, BUY, 300.waves, 0.35.wavesUsdPrice, matcherFee = 0.00370300.waves, matcherFeeAssetId = Waves)
    val sellOrder = mkOrder(bob, wavesUsdPair, SELL, 300.waves, 0.35.wavesUsdPrice, matcherFee = 0.30.usd, matcherFeeAssetId = usd)

    dex1Api.place(buyOrder)
    dex1Api.place(sellOrder)

    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.Filled)
    dex1Api.waitForOrderStatus(sellOrder, OrderStatus.Filled)

    retry(20, batchLingerMs) {
      withClue("checking info for counter order\n") {
        getOrderInfoById(buyOrder.id()).get shouldBe OrderBriefInfo(buyOrder.idStr(),
                                                                    limitOrderType,
                                                                    alice.publicKey.toString,
                                                                    buySide,
                                                                    "WAVES",
                                                                    stringify(usd),
                                                                    "WAVES",
                                                                    300,
                                                                    0.35,
                                                                    0.00370300)
        getEventsInfoByOrderId(buyOrder.id()) shouldBe Set {
          EventBriefInfo(buyOrder.idStr(), eventTrade, 300, 300, 0.00370300, 0.00370300, statusFilled)
        }
      }

      withClue("checking info for submitted order\n") {
        getOrderInfoById(sellOrder.id()).get shouldBe OrderBriefInfo(sellOrder.idStr(),
                                                                     limitOrderType,
                                                                     bob.publicKey.toString,
                                                                     sellSide,
                                                                     "WAVES",
                                                                     stringify(usd),
                                                                     stringify(usd),
                                                                     300,
                                                                     0.35,
                                                                     0.30)

        getEventsInfoByOrderId(sellOrder.id()) shouldBe Set {
          EventBriefInfo(sellOrder.idStr(), eventTrade, 300, 300, 0.30, 0.30, statusFilled)
        }
      }
    }
  }

  "Order history should correctly save events: 1 small counter and 1 big submitted" in {

    val smallBuyOrder = mkOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.wctUsdPrice, 0.00001703.eth, matcherFeeAssetId = eth)
    val bigSellOrder  = mkOrder(bob, wctUsdPair, SELL, 900.wct, 0.35.wctUsdPrice, 0.00000030.btc, matcherFeeAssetId = btc)

    dex1Api.place(smallBuyOrder)
    dex1Api.place(bigSellOrder)

    dex1Api.waitForOrderStatus(smallBuyOrder, OrderStatus.Filled)
    dex1Api.waitForOrderStatus(bigSellOrder, OrderStatus.PartiallyFilled)

    retry(20, batchLingerMs) {
      withClue("checking info for small counter order\n") {
        getOrderInfoById(smallBuyOrder.id()).get shouldBe
          OrderBriefInfo(smallBuyOrder.idStr(),
                         limitOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         300,
                         0.35,
                         0.00001703)

        getEventsInfoByOrderId(smallBuyOrder.id()) shouldBe Set {
          EventBriefInfo(smallBuyOrder.idStr(), eventTrade, 300, 300, 0.00001703, 0.00001703, statusFilled)
        }
      }

      withClue("checking info for big submitted order\n") {
        getOrderInfoById(bigSellOrder.id()).get shouldBe
          OrderBriefInfo(bigSellOrder.idStr(),
                         limitOrderType,
                         bob.publicKey.toString,
                         sellSide,
                         stringify(wct),
                         stringify(usd),
                         stringify(btc),
                         900,
                         0.35,
                         0.00000030)

        getEventsInfoByOrderId(bigSellOrder.id()) shouldBe Set {
          EventBriefInfo(bigSellOrder.idStr(), eventTrade, 300, 300, 0.00000010, 0.00000010, statusPartiallyFilled)
        }
      }
    }
  }

  "Order history should correctly save market orders and their events" in {
    dex1Api.cancelAll(bob)
    dex1Api.cancelAll(alice)

    def bigBuyOrder: Order = mkOrder(alice, wctUsdPair, BUY, 500.wct, 0.35.wctUsdPrice, matcherFee = 0.00001703.eth, matcherFeeAssetId = eth)

    withClue("place buy market order into empty order book") {

      val unmatchableMarketBuyOrder = bigBuyOrder
      dex1Api.placeMarket(unmatchableMarketBuyOrder)
      dex1Api.waitForOrder(unmatchableMarketBuyOrder)(_ == OrderStatusResponse(OrderStatus.Filled, Some(0.wct)))

      retry(20, batchLingerMs) {
        getOrderInfoById(unmatchableMarketBuyOrder.id()) shouldBe Some(
          OrderBriefInfo(unmatchableMarketBuyOrder.idStr(),
                         marketOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         500,
                         0.35,
                         0.00001703)
        )

        getEventsInfoByOrderId(unmatchableMarketBuyOrder.id()) shouldBe Set(
          EventBriefInfo(unmatchableMarketBuyOrder.idStr(), eventCancel, 0, 0, 0, 0, statusFilled)
        )
      }
    }

    withClue("place buy market order into nonempty order book") {

      val orders = Seq(
        mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.33.wctUsdPrice, 0.003.waves),
        mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.34.wctUsdPrice, 0.003.waves),
        mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.34.wctUsdPrice, 0.003.waves)
      )

      orders.foreach { order =>
        dex1Api.place(order)
        dex1Api.waitForOrderStatus(order, OrderStatus.Accepted)
      }

      val marketBuyOrder = bigBuyOrder
      dex1Api.placeMarket(marketBuyOrder)
      dex1Api.waitForOrder(marketBuyOrder)(_ == OrderStatusResponse(OrderStatus.Filled, Some(300.wct)))

      retry(15, batchLingerMs) {
        getOrderInfoById(marketBuyOrder.id()).get shouldBe
          OrderBriefInfo(marketBuyOrder.idStr(),
                         marketOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         500,
                         0.35,
                         0.00001703)

        getEventsInfoByOrderId(marketBuyOrder.id()) shouldBe
          Set(
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 100, 0.00000340, 0.00000340, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 200, 0.00000340, 0.00000680, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 300, 0.00000340, 0.00001020, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventCancel, 0, 300, 0, 0.00001020, statusFilled)
          )
      }
    }
  }
}
