package com.wavesplatform.it.sync

import java.net.InetAddress
import java.sql.{Connection, DriverManager}

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
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
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

  val batchLingerMs: Int = OrderHistorySettings.defaultBatchLingerMs

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
       |  }
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

    Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_)).foreach(tx => node.waitForTransaction(tx.id))
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

  case class OrderShortenedInfo(id: String, tpe: Byte, senderPublicKey: String, side: Byte, price: Double, amount: Double)
  case class EventShortenedInfo(orderId: String, eventType: Byte, filled: Double, totalFilled: Double, status: Byte)

  def getOrderInfoById(orderId: String): Option[OrderShortenedInfo] =
    ctx
      .run(
        querySchema[OrderShortenedInfo](
          "orders",
          _.id              -> "id",
          _.tpe             -> "type",
          _.senderPublicKey -> "sender_public_key",
          _.side            -> "side",
          _.price           -> "price",
          _.amount          -> "amount"
        ).filter(_.id == lift(orderId))
      )
      .headOption

  def getEventsInfoByOrderId(orderId: String): Set[EventShortenedInfo] =
    ctx
      .run(
        querySchema[EventShortenedInfo](
          "events",
          _.eventType   -> "event_type",
          _.filled      -> "filled",
          _.totalFilled -> "total_filled",
          _.status      -> "status"
        ).filter(_.orderId == lift(orderId))
      )
      .toSet

  import com.wavesplatform.dex.history.HistoryRouter._

  val (amount, price)            = (1000L, PriceConstant)
  val denormalizedAmount: Double = Denormalization.denormalizeAmountAndFee(amount, Decimals)
  val denormalizedPrice: Double  = Denormalization.denormalizePrice(price, Decimals, Decimals)

  "Order history should save all orders and events" in {
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

  "Order history should correctly save events for the big buy order" in {
    val buyOrder   = node.placeOrder(alice, wctUsdPair, BUY, 3 * amount, price, matcherFee).message.id
    val sellOrder1 = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder1, "Filled")

    val sellOrder2 = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder2, "Filled")

    node.cancelOrder(alice, wctUsdPair, buyOrder)

    retry(10, batchLingerMs) {

      getOrderInfoById(sellOrder1) shouldBe
        Some(OrderShortenedInfo(sellOrder1, limitOrderType, bob.publicKey.toString, sellSide, denormalizedPrice, denormalizedAmount))

      getEventsInfoByOrderId(buyOrder) shouldBe
        Set(
          EventShortenedInfo(buyOrder, eventTrade, denormalizedAmount, denormalizedAmount, statusPartiallyFilled),
          EventShortenedInfo(buyOrder, eventTrade, denormalizedAmount, 2 * denormalizedAmount, statusPartiallyFilled),
          EventShortenedInfo(buyOrder, eventCancel, 0, 2 * denormalizedAmount, statusCancelled)
        )
    }
  }

  "Order history should correctly save events for small and big orders" in {
    val smallBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id
    val bigSellOrder  = node.placeOrder(bob, wctUsdPair, SELL, 5 * amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, smallBuyOrder, "Filled")
    node.waitOrderStatus(wctUsdPair, bigSellOrder, "PartiallyFilled")

    retry(20, batchLingerMs) {

      getOrderInfoById(smallBuyOrder) shouldBe
        Some(OrderShortenedInfo(smallBuyOrder, limitOrderType, alice.publicKey.toString, buySide, denormalizedPrice, denormalizedAmount))

      getOrderInfoById(bigSellOrder) shouldBe
        Some(OrderShortenedInfo(bigSellOrder, limitOrderType, bob.publicKey.toString, sellSide, denormalizedPrice, 5 * denormalizedAmount))

      getEventsInfoByOrderId(smallBuyOrder).head shouldBe
        EventShortenedInfo(smallBuyOrder, eventTrade, denormalizedAmount, denormalizedAmount, statusFilled)

      getEventsInfoByOrderId(bigSellOrder).head shouldBe
        EventShortenedInfo(bigSellOrder, eventTrade, denormalizedAmount, denormalizedAmount, statusPartiallyFilled)
    }
  }

  "Order history should correctly save market orders and their events" in {

    node.cancelAllOrders(bob)
    node.cancelAllOrders(alice)

    def bigBuyOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, 5 * amount, price, matcherFee)

    withClue("place buy market order into empty order book") {

      val unmatchableMarketBuyOrder = node.placeMarketOrder(bigBuyOrder).message.id
      node.waitOrderStatus(wctUsdPair, unmatchableMarketBuyOrder, "Filled").filledAmount shouldBe Some(0)

      retry(20, batchLingerMs) {

        getOrderInfoById(unmatchableMarketBuyOrder).get shouldBe
          OrderShortenedInfo(unmatchableMarketBuyOrder, marketOrderType, alice.publicKey.toString, buySide, denormalizedPrice, 5 * denormalizedAmount)

        val marketOrderEvent = getEventsInfoByOrderId(unmatchableMarketBuyOrder)

        marketOrderEvent.size shouldBe 1
        marketOrderEvent.head shouldBe EventShortenedInfo(unmatchableMarketBuyOrder, eventCancel, 0, 0, statusCancelled) // FIXME DEX-339 (should be statusFilled)
      }
    }

    withClue("place buy market order into nonempty order book") {
      Seq(
        node.placeOrder(bob, wctUsdPair, SELL, amount, (price * 0.97).toLong, matcherFee).message.id,
        node.placeOrder(bob, wctUsdPair, SELL, amount, (price * 0.98).toLong, matcherFee).message.id,
        node.placeOrder(bob, wctUsdPair, SELL, amount, (price * 0.98).toLong, matcherFee).message.id
      ).foreach(lo => node.waitOrderStatus(wctUsdPair, lo, "Accepted"))

      val marketBuyOrder = node.placeMarketOrder(bigBuyOrder).message.id
      node.waitOrderStatus(wctUsdPair, marketBuyOrder, "Filled").filledAmount shouldBe Some(3 * amount)

      retry(15, batchLingerMs) {

        getOrderInfoById(marketBuyOrder) shouldBe
          Some(OrderShortenedInfo(marketBuyOrder, marketOrderType, alice.publicKey.toString, buySide, denormalizedPrice, 5 * denormalizedAmount))

        val marketOrderEvents = getEventsInfoByOrderId(marketBuyOrder)
        marketOrderEvents.size shouldBe 4

        marketOrderEvents shouldBe
          Set(
            EventShortenedInfo(marketBuyOrder, eventTrade, denormalizedAmount, 1 * denormalizedAmount, statusPartiallyFilled),
            EventShortenedInfo(marketBuyOrder, eventTrade, denormalizedAmount, 2 * denormalizedAmount, statusPartiallyFilled),
            EventShortenedInfo(marketBuyOrder, eventTrade, denormalizedAmount, 3 * denormalizedAmount, statusPartiallyFilled),
            EventShortenedInfo(marketBuyOrder, eventCancel, 0, 3 * denormalizedAmount, statusCancelled) // FIXME DEX-339 (should be statusFilled)
          )
      }
    }
  }
}
