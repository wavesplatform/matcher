package com.wavesplatform.it.sync

import java.sql.{Connection, DriverManager}
import java.util.concurrent.ThreadLocalRandom

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.it.tools.DockerContainerLauncher
import com.wavesplatform.dex.model.MatcherModel.Denormalization
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.{OrderStatus, OrderStatusResponse}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import io.getquill.{PostgresJdbcContext, SnakeCase}
import net.ceedubs.ficus.Ficus._

import scala.io.Source
import scala.util.{Failure, Try}

class OrderHistoryTestSuite extends NewMatcherSuiteBase {

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
  private val postgresContainerIp             = dockerClient().ipForNode(10)

  private val postgresContainerLauncher =
    new DockerContainerLauncher(
      imageName = postgresImageName,
      containerName = postgresContainerName,
      containerIp = postgresContainerIp,
      containerPort = postgresContainerPort,
      env = postgresEnv,
      networkName = dockerClient().network().name,
      imageTag = "10"
    )

  private val batchLingerMs: Int  = OrderHistorySettings.defaultBatchLingerMs
  private val ethAsset: Asset     = IssuedAsset(EthId)
  private val ethAssetStr: String = AssetPair.assetIdStr(ethAsset)

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

    broadcast(IssueUsdTx, IssueWctTx, IssueEthTx)
    dex1Api.upsertRate(ethAsset, 1.0)._1 shouldBe StatusCodes.Created
  }

  override protected def afterAll(): Unit = {
    postgresContainerLauncher.stopAndRemoveContainer()
    super.afterAll()
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
                                    price: Double,
                                    amount: Double,
                                    feeAsset: String = "WAVES")
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
          _.price           -> "price",
          _.amount          -> "amount",
          _.feeAsset        -> "fee_asset_id"
        ).filter(_.id == lift(orderId.toString))
      )
      .headOption

  private def getEventsInfoByOrderId(orderId: Order.Id): Set[EventBriefInfo] =
    ctx
      .run(
        querySchema[EventBriefInfo](
          "events",
          _.eventType   -> "event_type",
          _.filled      -> "filled",
          _.totalFilled -> "total_filled",
          _.status      -> "status"
        ).filter(_.orderId == lift(orderId.toString))
      )
      .toSet

  private val (amount, price) = (1000L, PriceConstant)
  private val dAmount: Double = Denormalization.denormalizeAmountAndFee(amount, Decimals)
  private val dPrice: Double  = Denormalization.denormalizePrice(price, Decimals, Decimals)
  private val dFee: Double    = Denormalization.denormalizeAmountAndFee(matcherFee, Decimals)

  "Order history should save all orders and events" in {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { _ =>
        dex1Api.place(mkOrder(alice, wctUsdPair, BUY, 1, price))
        dex1Api.place(mkOrder(bob, wctUsdPair, SELL, 1, price))
      }

    retry(10, batchLingerMs) {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events: 1 big counter and 2 small submitted" in {
    def mkSellOrder = mkOrder(bob, wctUsdPair, SELL, 1 * amount, price)

    val buyOrder = mkOrder(alice, wctUsdPair, BUY, 3 * amount, price)
    dex1Api.place(buyOrder)

    val sellOrder1 = mkSellOrder
    dex1Api.place(mkSellOrder)

    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1Api.waitForOrderStatus(sellOrder1, OrderStatus.Filled)

    val sellOrder2 = mkSellOrder
    dex1Api.place(sellOrder2)

    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1Api.waitForOrderStatus(sellOrder2, OrderStatus.Filled)

    dex1Api.cancel(alice, buyOrder)

    retry(10, batchLingerMs) {
      withClue("checking info (order and events) for 2 small submitted orders") {
        Set(sellOrder1, sellOrder2).foreach { order =>
          getOrderInfoById(order.id()) shouldBe Some(OrderBriefInfo(order.idStr(), limitOrderType, bob.publicKey.toString, sellSide, dPrice, dAmount))
          getEventsInfoByOrderId(order.id()) shouldBe Set(EventBriefInfo(order.idStr(), eventTrade, dAmount, dAmount, dFee, dFee, statusFilled))
        }
      }

      withClue("checking info (order and events) for 1 big counter order") {
        getOrderInfoById(buyOrder.id()) shouldBe Some(
          OrderBriefInfo(buyOrder.idStr(), limitOrderType, alice.publicKey.toString, buySide, dPrice, 3 * dAmount))
        getEventsInfoByOrderId(buyOrder.id()) shouldBe
          Set(
            EventBriefInfo(buyOrder.idStr(), eventTrade, 1 * dAmount, 1 * dAmount, 1 * dFee / 3, 1 * dFee / 3, statusPartiallyFilled),
            EventBriefInfo(buyOrder.idStr(), eventTrade, 1 * dAmount, 2 * dAmount, 1 * dFee / 3, 2 * dFee / 3, statusPartiallyFilled),
            EventBriefInfo(buyOrder.idStr(), eventCancel, 0 * dAmount, 2 * dAmount, 0 * dFee / 3, 2 * dFee / 3, statusCancelled)
          )
      }
    }
  }

  "Order history should correctly save events: 1 small counter and 1 big submitted" in {
    val smallBuyOrder = mkOrder(alice, wctUsdPair, BUY, 1 * amount, price)
    dex1Api.place(smallBuyOrder)

    val bigSellOrder = mkOrder(bob, wctUsdPair, SELL, 5 * amount, price)
    dex1Api.place(bigSellOrder)

    dex1Api.waitForOrderStatus(smallBuyOrder, OrderStatus.Filled)
    dex1Api.waitForOrderStatus(bigSellOrder, OrderStatus.PartiallyFilled)

    retry(20, batchLingerMs) {
      withClue("checking info (order and events) for 2 small counter order") {
        getOrderInfoById(smallBuyOrder.id()).get shouldBe OrderBriefInfo(smallBuyOrder.idStr(),
                                                                         limitOrderType,
                                                                         alice.publicKey.toString,
                                                                         buySide,
                                                                         dPrice,
                                                                         dAmount)
        getEventsInfoByOrderId(smallBuyOrder.id()) shouldBe Set(
          EventBriefInfo(smallBuyOrder.idStr(), eventTrade, dAmount, dAmount, dFee, dFee, statusFilled))
      }

      withClue("checking info (order and events) for 1 big submitted order") {
        getOrderInfoById(bigSellOrder.id()) shouldBe Some(
          OrderBriefInfo(bigSellOrder.idStr(), limitOrderType, bob.publicKey.toString, sellSide, dPrice, 5 * dAmount)
        )

        getEventsInfoByOrderId(bigSellOrder.id()) shouldBe Set(
          EventBriefInfo(bigSellOrder.idStr(), eventTrade, dAmount, dAmount, dFee / 5, dFee / 5, statusPartiallyFilled)
        )
      }
    }
  }

  "Order history should correctly save market orders and their events" in {
    dex1Api.cancelAll(bob)
    dex1Api.cancelAll(alice)

    def mkBigBuyOrder: Order = mkOrder(alice, wctUsdPair, BUY, 5 * amount, price, matcherFeeAssetId = ethAsset)

    withClue("place buy market order into empty order book") {
      val unmatchableMarketBuyOrder = mkBigBuyOrder
      dex1Api.placeMarket(unmatchableMarketBuyOrder)
      dex1Api.waitForOrder(unmatchableMarketBuyOrder)(_ == OrderStatusResponse(OrderStatus.Filled, Some(0)))

      retry(20, batchLingerMs) {
        getOrderInfoById(unmatchableMarketBuyOrder.id()) shouldBe Some(
          OrderBriefInfo(unmatchableMarketBuyOrder.idStr(), marketOrderType, alice.publicKey.toString, buySide, dPrice, 5 * dAmount, ethAssetStr)
        )

        getEventsInfoByOrderId(unmatchableMarketBuyOrder.id()) shouldBe Set(
          EventBriefInfo(unmatchableMarketBuyOrder.idStr(), eventCancel, 0, 0, 0, 0, statusFilled)
        )
      }
    }

    withClue("place buy market order into nonempty order book") {
      val orders = Seq(
        mkOrder(bob, wctUsdPair, SELL, amount, (price * 0.97).toLong),
        mkOrder(bob, wctUsdPair, SELL, amount, (price * 0.98).toLong),
        mkOrder(bob, wctUsdPair, SELL, amount, (price * 0.98).toLong)
      )
      orders.foreach(dex1Api.place)
      orders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Accepted))

      val marketBuyOrder = mkBigBuyOrder
      dex1Api.placeMarket(marketBuyOrder)
      dex1Api.waitForOrder(marketBuyOrder)(_ == OrderStatusResponse(OrderStatus.Filled, Some(3 * amount)))

      retry(15, batchLingerMs) {
        getOrderInfoById(marketBuyOrder.id()) shouldBe
          Some(
            OrderBriefInfo(marketBuyOrder.idStr(), marketOrderType, alice.publicKey.toString, buySide, dPrice, 5 * dAmount, ethAssetStr)
          )

        getEventsInfoByOrderId(marketBuyOrder.id()) shouldBe
          Set(
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 1 * dAmount, 1 * dAmount, 1 * dFee / 5, 1 * dFee / 5, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 1 * dAmount, 2 * dAmount, 1 * dFee / 5, 2 * dFee / 5, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 1 * dAmount, 3 * dAmount, 1 * dFee / 5, 3 * dFee / 5, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventCancel, 0 * dAmount, 3 * dAmount, 0 * dFee / 5, 3 * dFee / 5, statusFilled)
          )
      }
    }
  }
}
