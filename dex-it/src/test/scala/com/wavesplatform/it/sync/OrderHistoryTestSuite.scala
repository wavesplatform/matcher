package com.wavesplatform.it.sync

import java.net.InetAddress
import java.sql.{Connection, DriverManager}

import akka.http.scaladsl.model.StatusCodes.Created
import com.google.common.primitives.Ints
import com.spotify.docker.client.messages.Network
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.model.MatcherModel.Denormalization
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.{DockerContainerLauncher, MatcherSuiteBase}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import io.getquill.{PostgresJdbcContext, SnakeCase}
import net.ceedubs.ficus.Ficus._

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

  val wavesNetwork: Network = dockerSingleton().createNetwork

  val postgresImageName, postgresUser = "postgres"
  val postgresContainerName           = "pgc"
  val postgresPassword                = "docker"
  val postgresEnv                     = s"POSTGRES_PASSWORD=$postgresPassword"
  val postgresContainerPort           = "5432"
  val postgresContainerIp: String     = InetAddress.getByAddress(Ints.toByteArray(10 & 0xF | dockerSingleton().networkSeed)).getHostAddress

  val postgresContainerLauncher =
    new DockerContainerLauncher(
      postgresImageName,
      postgresContainerName,
      postgresEnv,
      postgresContainerIp,
      postgresContainerPort,
      wavesNetwork.name
    )

  val batchLingerMs: Int  = OrderHistorySettings.defaultBatchLingerMs
  val ethAsset: Asset     = IssuedAsset(EthId)
  val ethAssetStr: String = AssetPair.assetIdStr(ethAsset)

  def getPostgresContainerHostPort: String = postgresContainerLauncher.getHostPort.explicitGet()

  @annotation.tailrec
  private def retry[T](attemptsCount: Int, delayInMs: Int)(fn: => T): Try[T] = {
    Try { fn } match {
      case Failure(_) if attemptsCount > 1 => if (delayInMs != 0) Thread.sleep(delayInMs); retry(attemptsCount - 1, delayInMs)(fn)
      case Failure(ex)                     => throw ex
      case success                         => success
    }
  }

  def createTables(postgresAddress: String): Unit = {

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

  def getPostgresConnectionCfgString(serverName: String, port: String): String =
    s"""
       |postgres {
       |  server-name = $serverName
       |  port-number = $port
       |  user = $postgresUser
       |  password = $postgresPassword
       |  data-source-class-name = "org.postgresql.ds.PGSimpleDataSource"
       |}
    """.stripMargin

  def getOrdersHistoryCfgString(batchLingerMs: Long): String =
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

  override protected def nodeConfigs: Seq[Config] =
    super.nodeConfigs.map(
      ConfigFactory
        .parseString(getOrdersHistoryCfgString(batchLingerMs))
        .withFallback
    )

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    postgresContainerLauncher.startContainer()
    createTables(s"localhost:$getPostgresContainerHostPort")

    Seq(IssueUsdTx, IssueWctTx, IssueEthTx).map(_.json()).map(node.broadcastRequest(_)).foreach(tx => node.waitForTransaction(tx.id))
    node.upsertRate(ethAsset, 1.0, expectedStatusCode = Created)
  }

  override protected def afterAll(): Unit = {
    postgresContainerLauncher.stopAndRemoveContainer()
    super.afterAll()
  }

  lazy val ctx =
    new PostgresJdbcContext(
      SnakeCase,
      ConfigFactory
        .parseString(getPostgresConnectionCfgString("localhost", getPostgresContainerHostPort))
        .as[PostgresConnection]("postgres")
        .getConfig
    )

  import ctx._

  def getOrdersCount: Long = ctx.run(querySchema[OrderRecord]("orders", _.id      -> "id").size)
  def getEventsCount: Long = ctx.run(querySchema[EventRecord]("events", _.orderId -> "order_id").size)

  case class OrderBriefInfo(id: String, tpe: Byte, senderPublicKey: String, side: Byte, price: Double, amount: Double, feeAsset: String = "WAVES")
  case class EventBriefInfo(orderId: String,
                            eventType: Byte,
                            filled: Double,
                            totalFilled: Double,
                            feeFilled: Double,
                            feeTotalFilled: Double,
                            status: Byte)

  def getOrderInfoById(orderId: String): Option[OrderBriefInfo] =
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
        ).filter(_.id == lift(orderId))
      )
      .headOption

  def getEventsInfoByOrderId(orderId: String): Set[EventBriefInfo] =
    ctx
      .run(
        querySchema[EventBriefInfo](
          "events",
          _.eventType   -> "event_type",
          _.filled      -> "filled",
          _.totalFilled -> "total_filled",
          _.status      -> "status"
        ).filter(_.orderId == lift(orderId))
      )
      .toSet

  import com.wavesplatform.dex.history.HistoryRouter._

  val (amount, price) = (1000L, PriceConstant)
  val dAmount: Double = Denormalization.denormalizeAmountAndFee(amount, Decimals)
  val dPrice: Double  = Denormalization.denormalizePrice(price, Decimals, Decimals)
  val dFee: Double    = Denormalization.denormalizeAmountAndFee(matcherFee, Decimals)

  "Order history should save all orders and events" in  {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { _ =>
        node.placeOrder(alice, wctUsdPair, BUY, 1, price, matcherFee)
        node.placeOrder(bob, wctUsdPair, SELL, 1, price, matcherFee)
      }

    retry(10, batchLingerMs) {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events: 1 big counter and 2 small submitted" in {

    def sellOrder: Order = node.prepareOrder(bob, wctUsdPair, SELL, 1 * amount, price, matcherFee)
    val buyOrder         = node.placeOrder(alice, wctUsdPair, BUY, 3 * amount, price, matcherFee).message.id

    val sellOrder1 = node.placeOrder(sellOrder).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder1, "Filled")

    val sellOrder2 = node.placeOrder(sellOrder).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder2, "Filled")

    node.cancelOrder(alice, wctUsdPair, buyOrder)

    retry(10, batchLingerMs) {

      withClue("checking info (order and events) for 2 small submitted orders") {
        Set(sellOrder1, sellOrder2).foreach { orderId =>
          getOrderInfoById(orderId) shouldBe Some(OrderBriefInfo(orderId, limitOrderType, bob.publicKey.toString, sellSide, dPrice, dAmount))
          getEventsInfoByOrderId(orderId) shouldBe Set(EventBriefInfo(orderId, eventTrade, dAmount, dAmount, dFee, dFee, statusFilled))
        }
      }

      withClue("checking info (order and events) for 1 big counter order") {
        getOrderInfoById(buyOrder) shouldBe Some(OrderBriefInfo(buyOrder, limitOrderType, alice.publicKey.toString, buySide, dPrice, 3 * dAmount))
        getEventsInfoByOrderId(buyOrder) shouldBe
          Set(
            EventBriefInfo(buyOrder, eventTrade, 1 * dAmount, 1 * dAmount, 1 * dFee / 3, 1 * dFee / 3, statusPartiallyFilled),
            EventBriefInfo(buyOrder, eventTrade, 1 * dAmount, 2 * dAmount, 1 * dFee / 3, 2 * dFee / 3, statusPartiallyFilled),
            EventBriefInfo(buyOrder, eventCancel, 0 * dAmount, 2 * dAmount, 0 * dFee / 3, 2 * dFee / 3, statusCancelled)
          )
      }
    }
  }

  "Order history should correctly save events: 1 small counter and 1 big submitted" in {
    val smallBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, 1 * amount, price, matcherFee).message.id
    val bigSellOrder  = node.placeOrder(bob, wctUsdPair, SELL, 5 * amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, smallBuyOrder, "Filled")
    node.waitOrderStatus(wctUsdPair, bigSellOrder, "PartiallyFilled")

    retry(20, batchLingerMs) {

      withClue("checking info (order and events) for 2 small counter order") {
        getOrderInfoById(smallBuyOrder).get shouldBe OrderBriefInfo(smallBuyOrder, limitOrderType, alice.publicKey.toString, buySide, dPrice, dAmount)
        getEventsInfoByOrderId(smallBuyOrder) shouldBe Set(EventBriefInfo(smallBuyOrder, eventTrade, dAmount, dAmount, dFee, dFee, statusFilled))
      }

      withClue("checking info (order and events) for 1 big submitted order") {
        getOrderInfoById(bigSellOrder) shouldBe Some(
          OrderBriefInfo(bigSellOrder, limitOrderType, bob.publicKey.toString, sellSide, dPrice, 5 * dAmount)
        )

        getEventsInfoByOrderId(bigSellOrder) shouldBe Set(
          EventBriefInfo(bigSellOrder, eventTrade, dAmount, dAmount, dFee / 5, dFee / 5, statusPartiallyFilled)
        )
      }
    }
  }

  "Order history should correctly save market orders and their events" in {

    node.cancelAllOrders(bob)
    node.cancelAllOrders(alice)

    def bigBuyOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, 5 * amount, price, matcherFee, version = 3, matcherFeeAssetId = ethAsset)

    withClue("place buy market order into empty order book") {

      val unmatchableMarketBuyOrder = node.placeMarketOrder(bigBuyOrder).message.id
      node.waitOrderStatusAndAmount(wctUsdPair, unmatchableMarketBuyOrder, "Filled", Some(0))

      retry(20, batchLingerMs) {

        getOrderInfoById(unmatchableMarketBuyOrder) shouldBe Some(
          OrderBriefInfo(unmatchableMarketBuyOrder, marketOrderType, alice.publicKey.toString, buySide, dPrice, 5 * dAmount, ethAssetStr)
        )

        getEventsInfoByOrderId(unmatchableMarketBuyOrder) shouldBe Set(
          EventBriefInfo(unmatchableMarketBuyOrder, eventCancel, 0, 0, 0, 0, statusFilled)
        )
      }
    }

    withClue("place buy market order into nonempty order book") {
      Seq(
        node.placeOrder(bob, wctUsdPair, SELL, amount, (price * 0.97).toLong, matcherFee).message.id,
        node.placeOrder(bob, wctUsdPair, SELL, amount, (price * 0.98).toLong, matcherFee).message.id,
        node.placeOrder(bob, wctUsdPair, SELL, amount, (price * 0.98).toLong, matcherFee).message.id
      ).foreach(lo => node.waitOrderStatus(wctUsdPair, lo, "Accepted"))

      val marketBuyOrder = node.placeMarketOrder(bigBuyOrder).message.id
      node.waitOrderStatusAndAmount(wctUsdPair, marketBuyOrder, "Filled", Some(3 * amount))

      retry(15, batchLingerMs) {

        getOrderInfoById(marketBuyOrder) shouldBe
          Some(
            OrderBriefInfo(marketBuyOrder, marketOrderType, alice.publicKey.toString, buySide, dPrice, 5 * dAmount, ethAssetStr)
          )

        getEventsInfoByOrderId(marketBuyOrder) shouldBe
          Set(
            EventBriefInfo(marketBuyOrder, eventTrade, 1 * dAmount, 1 * dAmount, 1 * dFee / 5, 1 * dFee / 5, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder, eventTrade, 1 * dAmount, 2 * dAmount, 1 * dFee / 5, 2 * dFee / 5, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder, eventTrade, 1 * dAmount, 3 * dAmount, 1 * dFee / 5, 3 * dFee / 5, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder, eventCancel, 0 * dAmount, 3 * dAmount, 0 * dFee / 5, 3 * dFee / 5, statusFilled)
          )
      }
    }
  }

  "Order history should save fee info" in {
    withClue("in placed order") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe None
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly), node.ordersByAddress(alice, activeOnly), node.fullOrderHistory(alice, activeOnly)).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe 0
            orderbook.feeAsset shouldBe Waves
          }
        )
      }
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
    }

    withClue("in cancelled order") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe None
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 0
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair,activeOnly = true), node.ordersByAddress(alice, activeOnly = true), node.fullOrderHistory(alice, activeOnly = true)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderId) shouldBe None
      )
    }

    withClue("in filled orders") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id
      val bobOrderId =  node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderId)
      Array(bobOrderId, aliceOrderId).foreach((id: String) => node.orderStatus(id, wctUsdPair).filledFee shouldBe Some(matcherFee))
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair,activeOnly = true), node.ordersByAddress(alice, activeOnly = true), node.fullOrderHistory(alice, activeOnly = true)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderId) shouldBe None
      )
    }

    withClue("in partially filled orders") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
      val bobOrderId =  node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderId)
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly), node.ordersByAddress(alice, activeOnly), node.fullOrderHistory(alice, activeOnly)).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe matcherFee / 2
            orderbook.feeAsset shouldBe Waves
          }
        )
      }
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
    }

    withClue("in cancelled partially filled orders") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
      val bobOrderId =  node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderId)
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee / 2
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair,activeOnly = true), node.ordersByAddress(alice, activeOnly = true), node.fullOrderHistory(alice, activeOnly = true)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderId) shouldBe None
      )
    }

    withClue("in partially filled orders with fractional filled amount") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 9 * amount, price, matcherFee).message.id
      node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderId)
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe Some(33333)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 33333
          orderbook.feeAsset shouldBe Waves
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
    }
  }
}
