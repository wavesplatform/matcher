package com.wavesplatform.it.sync

import java.sql.{Connection, DriverManager}

import com.dimafeng.testcontainers.PostgreSQLContainer
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Normalization
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.it.api.responses.dex.{OrderStatus, OrderStatusResponse}
import com.wavesplatform.dex.it.docker.base.BaseContainer
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.PostgresConnection
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.it.MatcherSuiteBase
import io.getquill.{PostgresJdbcContext, SnakeCase}
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.Try

class PostgresHistoryDatabaseTestSuite extends MatcherSuiteBase {

  private val customDB       = "user_db"
  private val customUser     = "user"
  private val customPassword = "user"

  private val postgresContainerName = "pgc"
  private val postgresContainerPort = 5432

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES", "$EthId", "$WctId" ]
       |  ${getPostgresConnectionCfgString(postgresContainerName, postgresContainerPort)}
       |  order-history.enabled = yes
       |}
    """.stripMargin
  )

  private def getPostgresConnectionCfgString(serverName: String, port: Int): String =
    s"""
       |postgres {
       |  server-name = $serverName
       |  port-number = $port
       |  database = $customDB
       |  user = $customUser
       |  password = $customPassword
       |  data-source-class-name = "org.postgresql.ds.PGSimpleDataSource"
       |}
    """.stripMargin

  private val postgres: PostgreSQLContainer =
    PostgreSQLContainer(
      databaseName = customDB,
      username = customUser,
      password = customPassword
    ).configure { p =>
      p.withNetwork(BaseContainer.network)
      p.withNetworkAliases(postgresContainerName)
      p.withCreateContainerCmdModifier { cmd =>
        cmd withName postgresContainerName
        cmd withIpv4Address BaseContainer.getIp(11)
      }
    }

  private def createTables(): Unit = {

    val orderHistoryDDLFileName = "/order-history/order-history-ddl.sql"

    def getFileContentStr(fileName: String): String = {
      val fileStream = getClass.getResourceAsStream(fileName)
      Source.fromInputStream(fileStream).getLines.toSeq.mkString
    }

    def executeCreateTablesStatement(sqlConnection: Connection): Try[Unit] = Try {

      val createTablesDDL       = getFileContentStr(orderHistoryDDLFileName)
      val createTablesStatement = sqlConnection.prepareStatement(createTablesDDL)

      createTablesStatement.executeUpdate()
      createTablesStatement.close()
    }

    val sqlConnection = DriverManager.getConnection(postgres.jdbcUrl, postgres.username, postgres.password)
    executeCreateTablesStatement(sqlConnection).map(_ => sqlConnection.close())
  }

  override protected def beforeAll(): Unit = {
    // DEX depends on Postgres, so it must start before
    postgres.start()
    createTables()

    wavesNode1.start()

    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx)

    dex1.start()

    dex1.api.upsertRate(eth, 0.00567593)
    dex1.api.upsertRate(btc, 0.00009855)
    dex1.api.upsertRate(usd, 0.5)
  }

  private lazy val ctx =
    new PostgresJdbcContext(
      SnakeCase,
      ConfigFactory
        .parseString(getPostgresConnectionCfgString("localhost", postgres mappedPort postgresContainerPort))
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

  private def getEventsInfoByOrderId(orderId: Order.Id): List[EventBriefInfo] =
    ctx
      .run(
        querySchema[EventRecord](
          "events",
          _.eventType      -> "event_type",
          _.filled         -> "filled",
          _.totalFilled    -> "total_filled",
          _.feeFilled      -> "fee_filled",
          _.feeTotalFilled -> "fee_total_filled",
          _.status         -> "status",
          _.timestamp      -> "timestamp"
        ).filter(_.orderId == lift(orderId.toString))
      )
      .sortWith { (l, r) =>
        ((l.timestamp isBefore r.timestamp) || (l.timestamp isEqual r.timestamp)) &&
        (l.status <= r.status) &&
        (l.totalFilled <= r.totalFilled)
      }
      .map { r =>
        EventBriefInfo(r.orderId, r.eventType, r.filled.toDouble, r.totalFilled.toDouble, r.feeFilled.toDouble, r.feeTotalFilled.toDouble, r.status)
      }

  implicit class DoubleOps(value: Double) {
    val wct, usd: Long      = Normalization.normalizeAmountAndFee(value, 2)
    val eth, btc: Long      = Normalization.normalizeAmountAndFee(value, 8)
    val wctUsdPrice: Long   = Normalization.normalizePrice(value, 2, 2)
    val wavesUsdPrice: Long = Normalization.normalizePrice(value, 8, 2)
  }

  def stringify(asset: Asset): String = asset.toString

  "Order history should save all orders and events" in {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { i =>
        dex1.api.place(mkOrder(alice, wctUsdPair, BUY, 1.wct, 0.35.wctUsdPrice, 0.003.waves, ttl = 1.day + i.seconds))
        dex1.api.place(mkOrder(bob, wctUsdPair, SELL, 1.wct, 0.35.wctUsdPrice, 0.003.waves, ttl = 1.day + i.seconds))
      }

    eventually {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events: 1 big counter and 2 small submitted" in {

    def sellOrder: Order = mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.35.wctUsdPrice, matcherFee = 0.00000030.btc, feeAsset = btc)
    val buyOrder         = mkOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.wctUsdPrice, matcherFee = 0.00001703.eth, feeAsset = eth)

    dex1.api.place(buyOrder)

    val sellOrder1 = sellOrder
    dex1.api.place(sellOrder1)

    dex1.api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1.api.waitForOrderStatus(sellOrder1, OrderStatus.Filled)

    val sellOrder2 = sellOrder
    dex1.api.place(sellOrder2)

    dex1.api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1.api.waitForOrderStatus(sellOrder2, OrderStatus.Filled)

    dex1.api.cancel(alice, buyOrder)

    eventually {
      withClue("checking info for 2 small submitted orders\n") {

        Set(sellOrder1, sellOrder2).foreach { order =>
          getOrderInfoById(order.id()).get should matchTo(
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
          )

          getEventsInfoByOrderId(order.id()) should matchTo(
            List(EventBriefInfo(order.idStr(), eventTrade, 100, 100, 0.00000030, 0.00000030, statusFilled))
          )
        }
      }

      withClue("checking info for 1 big counter order\n") {
        getOrderInfoById(buyOrder.id()).get should matchTo(
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
        )

        getEventsInfoByOrderId(buyOrder.id()) should matchTo(
          List(
            EventBriefInfo(buyOrder.idStr(), eventTrade, 100, 100, 0.00000567, 0.00000567, statusPartiallyFilled),
            EventBriefInfo(buyOrder.idStr(), eventTrade, 100, 200, 0.00000567, 0.00001134, statusPartiallyFilled),
            EventBriefInfo(buyOrder.idStr(), eventCancel, 0, 200, 0, 0.00001134, statusCancelled)
          )
        )
      }
    }
  }

  "Order history should correctly save events with Waves as amount and fee" in {
    val buyOrder  = mkOrder(alice, wavesUsdPair, BUY, 300.waves, 0.35.wavesUsdPrice, matcherFee = 0.00370300.waves, feeAsset = Waves)
    val sellOrder = mkOrder(bob, wavesUsdPair, SELL, 300.waves, 0.35.wavesUsdPrice, matcherFee = 0.30.usd, feeAsset = usd)

    dex1.api.place(buyOrder)
    dex1.api.place(sellOrder)

    dex1.api.waitForOrderStatus(buyOrder, OrderStatus.Filled)
    dex1.api.waitForOrderStatus(sellOrder, OrderStatus.Filled)

    eventually {
      withClue("checking info for counter order\n") {
        getOrderInfoById(buyOrder.id()).get should matchTo(
          OrderBriefInfo(buyOrder.idStr(), limitOrderType, alice.publicKey.toString, buySide, "WAVES", stringify(usd), "WAVES", 300, 0.35, 0.00370300)
        )
        getEventsInfoByOrderId(buyOrder.id()) should matchTo(
          List(EventBriefInfo(buyOrder.idStr(), eventTrade, 300, 300, 0.00370300, 0.00370300, statusFilled))
        )
      }

      withClue("checking info for submitted order\n") {
        getOrderInfoById(sellOrder.id()).get should matchTo(
          OrderBriefInfo(sellOrder.idStr(),
                         limitOrderType,
                         bob.publicKey.toString,
                         sellSide,
                         "WAVES",
                         stringify(usd),
                         stringify(usd),
                         300,
                         0.35,
                         0.30)
        )

        getEventsInfoByOrderId(sellOrder.id()) should matchTo(
          List(EventBriefInfo(sellOrder.idStr(), eventTrade, 300, 300, 0.30, 0.30, statusFilled))
        )
      }
    }
  }

  "Order history should correctly save events: 1 small counter and 1 big submitted" in {

    val smallBuyOrder = mkOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.wctUsdPrice, 0.00001703.eth, feeAsset = eth)
    val bigSellOrder  = mkOrder(bob, wctUsdPair, SELL, 900.wct, 0.35.wctUsdPrice, 0.00000030.btc, feeAsset = btc)

    dex1.api.place(smallBuyOrder)
    dex1.api.place(bigSellOrder)

    dex1.api.waitForOrderStatus(smallBuyOrder, OrderStatus.Filled)
    dex1.api.waitForOrderStatus(bigSellOrder, OrderStatus.PartiallyFilled)

    eventually {
      withClue("checking info for small counter order\n") {
        getOrderInfoById(smallBuyOrder.id()).get should matchTo(
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
        )

        getEventsInfoByOrderId(smallBuyOrder.id()) should matchTo(
          List(EventBriefInfo(smallBuyOrder.idStr(), eventTrade, 300, 300, 0.00001703, 0.00001703, statusFilled))
        )
      }

      withClue("checking info for big submitted order\n") {
        getOrderInfoById(bigSellOrder.id()).get should matchTo(
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
        )

        getEventsInfoByOrderId(bigSellOrder.id()) should matchTo(
          List(EventBriefInfo(bigSellOrder.idStr(), eventTrade, 300, 300, 0.00000010, 0.00000010, statusPartiallyFilled))
        )
      }
    }
  }

  "Order history should correctly save market orders and their events" in {

    dex1.api.cancelAll(bob)
    dex1.api.cancelAll(alice)

    def bigBuyOrder: Order = mkOrder(alice, wctUsdPair, BUY, 500.wct, 0.35.wctUsdPrice, matcherFee = 0.00001703.eth, feeAsset = eth)

    withClue("place buy market order into empty order book") {

      val unmatchableMarketBuyOrder = bigBuyOrder
      dex1.api.placeMarket(unmatchableMarketBuyOrder)
      dex1.api.waitForOrder(unmatchableMarketBuyOrder)(
        _ == OrderStatusResponse(OrderStatus.Filled, filledAmount = Some(0.wct), filledFee = Some(0.wct)))

      eventually {
        getOrderInfoById(unmatchableMarketBuyOrder.id()).get should matchTo(
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

        getEventsInfoByOrderId(unmatchableMarketBuyOrder.id()) should matchTo(
          List(EventBriefInfo(unmatchableMarketBuyOrder.idStr(), eventCancel, 0, 0, 0, 0, statusFilled))
        )
      }
    }

    withClue("place buy market order into nonempty order book") {

      val ts = System.currentTimeMillis()

      val orders = Seq(
        mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.33.wctUsdPrice, 0.003.waves, ts = ts),
        mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.34.wctUsdPrice, 0.003.waves, ts = ts + 100),
        mkOrder(bob, wctUsdPair, SELL, 100.wct, 0.34.wctUsdPrice, 0.003.waves, ts = ts + 200)
      )

      orders.foreach { order =>
        dex1.api.place(order)
        dex1.api.waitForOrderStatus(order, OrderStatus.Accepted)
      }

      val marketBuyOrder = bigBuyOrder
      dex1.api.placeMarket(marketBuyOrder)
      dex1.api.waitForOrder(marketBuyOrder)(_ == OrderStatusResponse(OrderStatus.Filled, filledAmount = Some(300.wct), filledFee = Some(1020L)))

      eventually {
        getOrderInfoById(marketBuyOrder.id()).get should matchTo(
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
        )

        getEventsInfoByOrderId(marketBuyOrder.id()) should matchTo(
          List(
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 100, 0.00000340, 0.00000340, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 200, 0.00000340, 0.00000680, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventTrade, 100, 300, 0.00000340, 0.00001020, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder.idStr(), eventCancel, 0, 300, 0, 0.00001020, statusFilled)
          )
        )
      }
    }
  }
}
