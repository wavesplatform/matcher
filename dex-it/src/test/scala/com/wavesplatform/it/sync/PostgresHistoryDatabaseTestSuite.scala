package com.wavesplatform.it.sync

import java.sql.{Connection, DriverManager}
import java.time.LocalDateTime
import cats.syntax.option._
import com.dimafeng.testcontainers.PostgreSQLContainer
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.history.HasPostgresJdbcContext
import com.wavesplatform.dex.history.HistoryRouterActor._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.{EventReason, OrderCanceledReason, OrderExecutedReason}
import com.wavesplatform.dex.settings.PostgresConnection
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.OptionValues
import org.scalatest.concurrent.PatienceConfiguration.{Interval, Timeout}
import pureconfig.ConfigSource
import pureconfig.generic.auto._

import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.{Try, Using}

class PostgresHistoryDatabaseTestSuite extends MatcherSuiteBase with HasPostgresJdbcContext with OptionValues {

  override def connectionConfig: Config = ConfigSource
    .string(getPostgresConnectionCfgString("localhost", postgres mappedPort postgresContainerPort, "CheckPostgresConnectionApp"))
    .at("postgres")
    .loadOrThrow[PostgresConnection]
    .getQuillContextConfig

  private val customDB: String = "user_db"
  private val customUser: String = "user"
  private val customPassword: String = "user"
  private val customAppName: String = "TestApplicationName"

  private val postgresContainerName = "pgc"
  private val postgresContainerPort = 5432

  private val maxOrders = 99

  private val baseConf = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES", "$EthId", "$WctId" ]
       |  ${getPostgresConnectionCfgString(postgresContainerName, postgresContainerPort, customAppName)}
       |  order-db.max-orders = $maxOrders
       |  order-history.enable = yes
       |}
    """.stripMargin
  )

  override protected val dexInitialSuiteConfig: Config = baseConf.withFallback(mkCompositeDynamicFeeSettings(BtcId))

  private def getPostgresConnectionCfgString(serverName: String, port: Int, appName: String): String =
    s"""
       |postgres {
       |  server-name = $serverName
       |  port-number = $port
       |  database = $customDB
       |  user = $customUser
       |  password = $customPassword
       |  data-source-class-name = "org.postgresql.ds.PGSimpleDataSource"
       |  application-name = $appName
       |}
    """.stripMargin

  private lazy val postgres: PostgreSQLContainer = PostgreSQLContainer().configure { p =>
    p.withDatabaseName(customDB)
    p.withUsername(customUser)
    p.withPassword(customPassword)

    p.withNetwork(network)
    p.withNetworkAliases(postgresContainerName)
    p.withCreateContainerCmdModifier { cmd =>
      cmd withName postgresContainerName
      cmd withIpv4Address getIp(11)
    }
  }

  private def createTables(): Unit = {
    log.info("Creating tables")
    val orderHistoryDDLFileName = "/order-history/order-history-ddl.sql"

    def getFileContentStr(fileName: String): String = {
      val fileStream = getClass.getResourceAsStream(fileName)
      Source.fromInputStream(fileStream).getLines().mkString
    }

    def executeCreateTablesStatement(sqlConnection: Connection): Try[Unit] = Try {
      val createTablesDDL = getFileContentStr(orderHistoryDDLFileName)
      Using.resource(sqlConnection.prepareStatement(createTablesDDL)) { createTablesStatement =>
        createTablesStatement.executeUpdate()
      }
    }

    Using.resource(DriverManager.getConnection(postgres.jdbcUrl, postgres.username, postgres.password)) { sqlConnection =>
      executeCreateTablesStatement(sqlConnection).get // Force throw
    }
    log.info("Tables created")
  }

  override protected def beforeAll(): Unit = {
    // DEX depends on Postgres, so it must start before
    postgres.start()
    createTables()

    wavesNode1.start()

    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx)
    broadcastAndAwait(
      mkTransfer(bob, alice, 100.btc, btc),
      mkTransfer(alice, bob, 100.usd, usd),
      mkTransfer(alice, bob, 100.eth, eth)
    )

    dex1.start()

    dex1.api.upsertAssetRate(eth, 0.00567593)
    dex1.api.upsertAssetRate(btc, 0.00009855)
    dex1.api.upsertAssetRate(usd, 0.5)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    Seq(alice, bob).foreach(dex1.api.cancelAllOrdersWithSig(_))
  }

  import ctx._
  private def getOrdersCount: Long = ctx.run(querySchema[OrderRecord]("orders", _.id -> "id").size)
  private def getEventsCount: Long = ctx.run(querySchema[EventRecord]("events", _.orderId -> "order_id").size)

  private case class OrderBriefInfo(
    id: String,
    tpe: Byte,
    senderPublicKey: String,
    side: Byte,
    amountAsset: String,
    priceAsset: String,
    feeAsset: String,
    amount: Double,
    price: Double,
    fee: Double,
    closedAt: Option[LocalDateTime]
  )

  private case class EventBriefInfo(
    orderId: String,
    eventType: Byte,
    filled: Double,
    totalFilled: Double,
    feeFilled: Double,
    feeTotalFilled: Double,
    status: Byte,
    reason: EventReason = Events.NotTracked
  )

  private def getOrderInfoById(orderId: Order.Id): Option[OrderBriefInfo] =
    ctx
      .run(
        querySchema[OrderBriefInfo](
          "orders",
          _.id -> "id",
          _.tpe -> "type",
          _.senderPublicKey -> "sender_public_key",
          _.side -> "side",
          _.amountAsset -> "amount_asset_id",
          _.priceAsset -> "price_asset_id",
          _.feeAsset -> "fee_asset_id",
          _.amount -> "amount",
          _.price -> "price",
          _.fee -> "fee",
          _.closedAt -> "closed_at"
        ).filter(_.id == lift(orderId.toString))
      )
      .headOption

  private def getEventsInfoByOrderId(orderId: Order.Id): List[EventBriefInfo] =
    ctx
      .run(
        querySchema[EventRecord](
          "events",
          _.eventType -> "event_type",
          _.filled -> "filled",
          _.totalFilled -> "total_filled",
          _.feeFilled -> "fee_filled",
          _.feeTotalFilled -> "fee_total_filled",
          _.status -> "status",
          _.timestamp -> "timestamp",
          _.reason -> "reason"
        ).filter(_.orderId == lift(orderId.toString))
      )
      .sortWith { (l, r) =>
        ((l.timestamp isBefore r.timestamp) || (l.timestamp isEqual r.timestamp)) &&
        (l.status <= r.status) &&
        (l.totalFilled <= r.totalFilled)
      }
      .map { r =>
        EventBriefInfo(
          r.orderId,
          r.eventType,
          r.filled.toDouble,
          r.totalFilled.toDouble,
          r.feeFilled.toDouble,
          r.feeTotalFilled.toDouble,
          r.status,
          r.reason
        )
      }

  private def cleanTables(): Unit = {
    ctx.run(querySchema[OrderRecord]("orders").delete)
    ctx.run(querySchema[EventRecord]("events").delete)
  }

  "Node should provide its name to postgres" in {

    placeAndAwaitAtDex(mkOrder(bob, wavesBtcPair, BUY, 25.waves, 20.btc))
    placeAndAwaitAtNode(mkOrder(bob, wavesBtcPair, SELL, 25.waves, 20.btc))

    findUserWithApplicationName().getOrElse(PgStatActivity) shouldBe PgStatActivity(customUser, customAppName)

    cleanTables()
  }

  "Postgres order history should save correct filled status of closed orders" in {

    val buyOrder = mkOrder(bob, wavesBtcPair, BUY, 1.waves, 0.02.btc)
    val sellOrder = mkOrder(alice, wavesBtcPair, SELL, 1.5.waves, 0.02.btc)

    placeAndAwaitAtDex(buyOrder)
    placeAndAwaitAtNode(sellOrder)

    // buy counter order is not executed completely, but has filled status
    dex1.api.orderStatusByAssetPairAndId(buyOrder) should matchTo(HttpOrderStatus(Status.Filled, 1.waves.some, 0.003.btc.some))

    eventually {
      val buyOrderEvents = getEventsInfoByOrderId(buyOrder.id())
      buyOrderEvents should have size 1
      buyOrderEvents.head.status shouldBe statusFilled
    }

    getEventsInfoByOrderId(sellOrder.id()).last.status shouldBe statusPartiallyFilled

    Seq(alice, bob).foreach(dex1.api.cancelAllOrdersWithSig(_))
    cleanTables()
  }

  "Postgres order history should update closedAt timestamps of orders after" - {

    "cancel" in {

      val cancelledOrder = mkOrderDP(alice, wavesUsdPair, BUY, 1.waves, 3.50)

      placeAndAwaitAtDex(cancelledOrder)
      eventually(getOrderInfoById(cancelledOrder.id()).get.closedAt shouldBe None)

      cancelAndAwait(alice, cancelledOrder)
      eventually(getOrderInfoById(cancelledOrder.id()).get.closedAt.nonEmpty shouldBe true)

      cleanTables()
    }

    "execution" in {

      val counter = mkOrderDP(alice, wavesUsdPair, BUY, 1.waves, 3.50)
      val submitted = mkOrderDP(alice, wavesUsdPair, SELL, 1.waves, 3.50)

      placeAndAwaitAtDex(counter)
      placeAndAwaitAtNode(submitted)

      val filledEventTimestamp = eventually(expectFinalization(submitted.idStr()))
      eventually {
        Seq(counter, submitted).foreach { order =>
          getOrderInfoById(order.id()).get.closedAt should matchTo(filledEventTimestamp)
        }
      }

      cleanTables()
    }

    "expiration" in {
      val order = mkOrderDP(alice, wavesUsdPair, BUY, 1.waves, 3.50, ttl = 1.minute + 5.seconds) // We can't set ttl < 1.minute
      placeAndAwaitAtDex(order)

      // Because we have to wait more than 30 seconds
      eventually(timeout = Timeout(90.seconds), interval = Interval(1.second)) {
        dex1.api.orderStatusByAssetPairAndId(order).status shouldBe Status.Cancelled
      }

      eventually {
        val filledEventTimestamp = expectFinalization(order.idStr())
        getOrderInfoById(order.id()).get.closedAt should matchTo(filledEventTimestamp)
      }

      cleanTables()
    }
  }

  "Postgres order history should save all orders and events" in {
    (1 to maxOrders)
      .foreach { i =>
        dex1.api.place(mkOrderDP(alice, wctUsdPair, BUY, 1.wct, 0.35, 0.003.waves, ttl = 1.day + i.seconds))
        dex1.api.place(mkOrderDP(bob, wctUsdPair, SELL, 1.wct, 0.35, 0.003.waves, ttl = 1.day + i.seconds))
      }

    eventually {
      getOrdersCount shouldBe maxOrders * 2
      getEventsCount shouldBe maxOrders * 2
    }
  }

  "Postgres order history should correctly save events: 1 big counter and 2 small submitted" in {

    def sellOrder: Order = mkOrderDP(bob, wctUsdPair, SELL, 100.wct, 0.35, matcherFee = 0.00000030.btc, feeAsset = btc)
    val buyOrder = mkOrderDP(alice, wctUsdPair, BUY, 300.wct, 0.35, matcherFee = 0.00000030.btc, feeAsset = btc)

    dex1.api.place(buyOrder)

    val sellOrder1 = sellOrder
    dex1.api.place(sellOrder1)

    dex1.api.waitForOrderStatus(buyOrder, Status.PartiallyFilled)
    dex1.api.waitForOrderStatus(sellOrder1, Status.Filled)

    val sellOrder2 = sellOrder
    dex1.api.place(sellOrder2)

    dex1.api.waitForOrderStatus(buyOrder, Status.PartiallyFilled)
    dex1.api.waitForOrderStatus(sellOrder2, Status.Filled)

    dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, buyOrder)

    withClue("checking info for 2 small submitted orders\n") {
      Set(sellOrder1, sellOrder2).foreach { order =>
        eventually {
          val finalizeTimestamp = expectFinalization(order.idStr())
          withClue(s"${order.id()}\n") {
            getOrderInfoById(order.id()).get should matchTo(
              OrderBriefInfo(
                order.idStr(),
                limitOrderType,
                bob.publicKey.toString,
                sellSide,
                wct.toString,
                usd.toString,
                btc.toString,
                100,
                0.35,
                0.00000030,
                finalizeTimestamp
              )
            )

            getEventsInfoByOrderId(order.id()) should matchTo(
              List(EventBriefInfo(order.idStr(), eventTrade, 100, 100, 0.00000030, 0.00000030, statusFilled, OrderExecutedReason))
            )
          }
        }
      }

      withClue("checking info for 1 big counter order\n") {
        eventually {
          val finalizeTimestamp = expectFinalization(buyOrder.idStr())
          getOrderInfoById(buyOrder.id()).get should matchTo(
            OrderBriefInfo(
              buyOrder.idStr(),
              limitOrderType,
              alice.publicKey.toString,
              buySide,
              wct.toString,
              usd.toString,
              btc.toString,
              300,
              0.35,
              0.00000030,
              finalizeTimestamp
            )
          )
        }

        getEventsInfoByOrderId(buyOrder.id()) should matchTo(
          List(
            EventBriefInfo(buyOrder.idStr(), eventTrade, 100, 100, 0.0000001, 0.0000001, statusPartiallyFilled, OrderExecutedReason),
            EventBriefInfo(buyOrder.idStr(), eventTrade, 100, 200, 0.0000001, 0.0000002, statusPartiallyFilled, OrderExecutedReason),
            EventBriefInfo(buyOrder.idStr(), eventCancel, 0, 200, 0, 0.0000002, statusCancelled, OrderCanceledReason.RequestExecuted)
          )
        )
      }
    }
  }

  "Postgres order history should correctly save events with Waves as amount and fee" in {
    dex1.restartWithNewSuiteConfig(baseConf.withFallback(mkCompositeDynamicFeeSettings(UsdId)))
    val buyOrder = mkOrderDP(alice, wavesUsdPair, BUY, 300.waves, 0.35, matcherFee = 0.00370300.waves, feeAsset = Waves)
    val sellOrder = mkOrderDP(bob, wavesUsdPair, SELL, 300.waves, 0.35, matcherFee = 0.30.usd, feeAsset = usd)

    dex1.api.place(buyOrder)
    dex1.api.place(sellOrder)

    dex1.api.waitForOrderStatus(buyOrder, Status.Filled)
    dex1.api.waitForOrderStatus(sellOrder, Status.Filled)

    withClue("checking info for counter order\n") {
      eventually {
        val finalizeTimestamp = expectFinalization(buyOrder.idStr())
        getOrderInfoById(buyOrder.id()).get should matchTo(
          OrderBriefInfo(
            buyOrder.idStr(),
            limitOrderType,
            alice.publicKey.toString,
            buySide,
            "WAVES",
            usd.toString,
            "WAVES",
            300,
            0.35,
            0.00370300,
            finalizeTimestamp
          )
        )
        getEventsInfoByOrderId(buyOrder.id()) should matchTo(
          List(EventBriefInfo(buyOrder.idStr(), eventTrade, 300, 300, 0.00370300, 0.00370300, statusFilled, OrderExecutedReason))
        )
      }
    }

    withClue("checking info for submitted order\n") {
      eventually {
        val finalizeTimestamp = expectFinalization(sellOrder.idStr())
        getOrderInfoById(sellOrder.id()).get should matchTo(
          OrderBriefInfo(
            sellOrder.idStr(),
            limitOrderType,
            bob.publicKey.toString,
            sellSide,
            "WAVES",
            usd.toString,
            usd.toString,
            300,
            0.35,
            0.30,
            finalizeTimestamp
          )
        )

        getEventsInfoByOrderId(sellOrder.id()) should matchTo(
          List(EventBriefInfo(sellOrder.idStr(), eventTrade, 300, 300, 0.30, 0.30, statusFilled, OrderExecutedReason))
        )
      }
    }
  }

  "Postgres order history should correctly save events: 1 small counter and 1 big submitted" in {
    dex1.restartWithNewSuiteConfig(baseConf.withFallback(mkCompositeDynamicFeeSettings(EthId)))
    val smallBuyOrder = mkOrderDP(alice, wctUsdPair, BUY, 300.wct, 0.35, 0.00001703.eth, feeAsset = eth)
    val bigSellOrder = mkOrderDP(bob, wctUsdPair, SELL, 900.wct, 0.35, 0.00001704.eth, feeAsset = eth)

    dex1.api.place(smallBuyOrder)
    dex1.api.place(bigSellOrder)

    dex1.api.waitForOrderStatus(smallBuyOrder, Status.Filled)
    dex1.api.waitForOrderStatus(bigSellOrder, Status.PartiallyFilled)

    withClue("checking info for small counter order\n") {
      eventually {
        val finalizeTimestamp = expectFinalization(smallBuyOrder.idStr())
        getOrderInfoById(smallBuyOrder.id()).get should matchTo(
          OrderBriefInfo(
            smallBuyOrder.idStr(),
            limitOrderType,
            alice.publicKey.toString,
            buySide,
            wct.toString,
            usd.toString,
            eth.toString,
            300,
            0.35,
            0.00001703,
            finalizeTimestamp
          )
        )

        getEventsInfoByOrderId(smallBuyOrder.id()) should matchTo(
          List(EventBriefInfo(smallBuyOrder.idStr(), eventTrade, 300, 300, 0.00001703, 0.00001703, statusFilled, OrderExecutedReason))
        )
      }
    }

    withClue("checking info for big submitted order\n") {
      eventually {
        getOrderInfoById(bigSellOrder.id()).get should matchTo(
          OrderBriefInfo(
            bigSellOrder.idStr(),
            limitOrderType,
            bob.publicKey.toString,
            sellSide,
            wct.toString,
            usd.toString,
            eth.toString,
            900,
            0.35,
            0.00001704,
            None
          )
        )

        getEventsInfoByOrderId(bigSellOrder.id()) should matchTo(
          List(EventBriefInfo(bigSellOrder.idStr(), eventTrade, 300, 300, 0.00000568, 0.00000568, statusPartiallyFilled, OrderExecutedReason))
        )
      }
    }
  }

  "Postgres order history should correctly save market orders and their events" in {

    def bigBuyOrder: Order = mkOrderDP(alice, wctUsdPair, BUY, 500.wct, 0.35, matcherFee = 0.00001703.eth, feeAsset = eth)

    withClue("place buy market order into empty order book") {

      val unmatchableMarketBuyOrder = bigBuyOrder

      dex1.api.placeMarket(unmatchableMarketBuyOrder)
      dex1.api.waitForOrder(unmatchableMarketBuyOrder)(
        _ == HttpOrderStatus(Status.Filled, filledAmount = Some(0.wct), filledFee = Some(0.wct))
      )

      eventually {
        val finalizeTimestamp = expectFinalization(unmatchableMarketBuyOrder.idStr())
        getOrderInfoById(unmatchableMarketBuyOrder.id()).get should matchTo(
          OrderBriefInfo(
            unmatchableMarketBuyOrder.idStr(),
            marketOrderType,
            alice.publicKey.toString,
            buySide,
            wct.toString,
            usd.toString,
            eth.toString,
            500,
            0.35,
            0.00001703,
            finalizeTimestamp
          )
        )

        getEventsInfoByOrderId(unmatchableMarketBuyOrder.id()) should matchTo(
          List(EventBriefInfo(unmatchableMarketBuyOrder.idStr(), eventCancel, 0, 0, 0, 0, statusFilled, OrderCanceledReason.BecameUnmatchable))
        )
      }
    }

    withClue("place buy market order into nonempty order book") {

      val ts = System.currentTimeMillis()

      val orders = Seq(
        mkOrderDP(bob, wctUsdPair, SELL, 100.wct, 0.33, 0.003.waves, ts = ts),
        mkOrderDP(bob, wctUsdPair, SELL, 100.wct, 0.34, 0.003.waves, ts = ts + 100),
        mkOrderDP(bob, wctUsdPair, SELL, 100.wct, 0.34, 0.003.waves, ts = ts + 200)
      )

      orders.foreach { order =>
        dex1.api.place(order)
        dex1.api.waitForOrderStatus(order, Status.Accepted)
      }

      val marketBuyOrder = bigBuyOrder
      dex1.api.placeMarket(marketBuyOrder)
      dex1.api.waitForOrder(marketBuyOrder)(_ == HttpOrderStatus(Status.Filled, filledAmount = Some(300.wct), filledFee = Some(1020L)))

      eventually {
        val finalizeTimestamp = expectFinalization(marketBuyOrder.idStr())
        getOrderInfoById(marketBuyOrder.id()).get should matchTo(
          OrderBriefInfo(
            marketBuyOrder.idStr(),
            marketOrderType,
            alice.publicKey.toString,
            buySide,
            wct.toString,
            usd.toString,
            eth.toString,
            500,
            0.35,
            0.00001703,
            finalizeTimestamp
          )
        )

        getEventsInfoByOrderId(marketBuyOrder.id()) should matchTo(
          List(
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 100, 0.00000340, 0.00000340, statusPartiallyFilled, OrderExecutedReason),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 200, 0.00000340, 0.00000680, statusPartiallyFilled, OrderExecutedReason),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 300, 0.00000340, 0.00001020, statusPartiallyFilled, OrderExecutedReason),
            EventBriefInfo(marketBuyOrder.idStr(), eventCancel, 0, 300, 0, 0.00001020, statusFilled, OrderCanceledReason.BecameUnmatchable)
          )
        )
      }
    }
  }

  "Postgres order history should save orders that are filled with rounding issues" in {
    Seq(limitOrderType, marketOrderType).foreach { orderType =>
      val buyOrder = mkOrderDP(alice, wavesUsdPair, BUY, 1.23456789.waves, 1.03)

      dex1.api.place(mkOrderDP(bob, wavesUsdPair, SELL, 2.waves, 1.03))
      if (orderType == limitOrderType) dex1.api.place(buyOrder) else dex1.api.placeMarket(buyOrder)
      dex1.api.waitForOrderStatus(buyOrder, Status.Filled)

      withClue(s"${if (orderType == limitOrderType) "limit" else "market"} ${buyOrder.idStr()}\n") {
        eventually {
          val finalizeTimestamp = expectFinalization(buyOrder.idStr())
          getOrderInfoById(buyOrder.id()).get should matchTo(
            OrderBriefInfo(
              buyOrder.idStr(),
              orderType,
              alice.publicKey.toString,
              buySide,
              Waves.toString,
              usd.toString,
              Waves.toString,
              1.23456789,
              1.03,
              0.003,
              finalizeTimestamp
            )
          )
        }
      }

      Seq(alice, bob).foreach(dex1.api.cancelAllOrdersWithSig(_))
    }
  }

  private def expectFinalization(orderId: String): Option[LocalDateTime] = withClue(s"finalization of $orderId\n") {
    val r = finalizeEventTimestampOf(orderId)
    r should not be empty
    r
  }

  private val finalizeEventStatus = List(statusFilled, statusCancelled)

  private def finalizeEventTimestampOf(orderId: String): Option[LocalDateTime] =
    ctx
      .run(
        querySchema[EventRecord](
          "events",
          _.eventType -> "event_type",
          _.status -> "status"
        ).filter(record => record.orderId == lift(orderId) && liftQuery(finalizeEventStatus).contains(record.status))
      )
      .map(_.timestamp)
      .headOption

  private def findUserWithApplicationName(): Option[PgStatActivity] =
    ctx
      .run(
        querySchema[PgStatActivity](
          "pg_stat_activity",
          _.userName -> "usename",
          _.applicationName -> "application_name"
        ).filter(_.applicationName == lift(customAppName))
      ).headOption

}

final case class PgStatActivity(userName: String = "", applicationName: String = "")
