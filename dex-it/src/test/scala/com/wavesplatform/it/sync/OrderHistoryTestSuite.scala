package com.wavesplatform.it.sync

import java.net.InetAddress
import java.nio.file.Paths
import java.sql.{Connection, DriverManager}

import akka.http.scaladsl.model.StatusCodes
import com.google.common.primitives.Ints
import com.spotify.docker.client.messages.Network
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.it.{DockerContainerLauncher, MatcherSuiteBase}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
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

  val customDB       = "user_db"
  val customUser     = "user"
  val customPassword = "user"

  val postgresImageName           = "postgres"
  val postgresContainerName       = "pgc"
  val postgresContainerPort       = "5432"
  val postgresContainerIp: String = InetAddress.getByAddress(Ints.toByteArray(10 & 0xF | dockerSingleton().networkSeed)).getHostAddress

  val postgresContainerLauncher =
    new DockerContainerLauncher(
      postgresImageName,
      postgresContainerName,
      List(s"POSTGRES_DB=$customDB", s"POSTGRES_USER=$customUser", s"POSTGRES_PASSWORD=$customPassword"),
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

  def getFileContentStr(fileName: String): String = {
    val fileStream = getClass.getResourceAsStream(fileName)
    Source.fromInputStream(fileStream).getLines.toSeq.mkString
  }

  def createDatabase(): Unit = {

    val createDatabaseFileName    = "init-user-db.sh"
    val createDatabaseFileContent = getFileContentStr(s"/order-history/$createDatabaseFileName")

    postgresContainerLauncher.writeFile(
      to = Paths.get(s"/docker-entrypoint-initdb.d/$createDatabaseFileName"),
      content = createDatabaseFileContent
    )
  }

  def createTables(postgresAddress: String): Unit = {

    val url                     = s"jdbc:postgresql://$postgresAddress/$customDB"
    val orderHistoryDDLFileName = "/order-history/order-history-ddl.sql"

    def executeCreateTablesStatement(sqlConnection: Connection): Try[Unit] = Try {

      val createTablesDDL       = getFileContentStr(orderHistoryDDLFileName)
      val createTablesStatement = sqlConnection.prepareStatement(createTablesDDL)

      createTablesStatement.executeUpdate()
      createTablesStatement.close()
    }

    retry(10, 2000) { DriverManager.getConnection(url, customUser, customPassword) } flatMap { sqlConnection =>
      executeCreateTablesStatement(sqlConnection).map(_ => sqlConnection.close())
    } get
  }

  def getPostgresConnectionCfgString(serverName: String, port: String): String =
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

    createDatabase()
    postgresContainerLauncher.startContainer()
    createTables(s"localhost:$getPostgresContainerHostPort")

    Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).foreach { tx =>
      node.waitForTransaction { node.broadcastRequest(tx.json.value).id }
    }

    node.upsertRate(eth, 0.00567593, expectedStatusCode = StatusCodes.Created)
    node.upsertRate(btc, 0.00009855, expectedStatusCode = StatusCodes.Created)
    node.upsertRate(usd, 0.5, expectedStatusCode = StatusCodes.Created)
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

  case class OrderBriefInfo(id: String,
                            tpe: Byte,
                            senderPublicKey: String,
                            side: Byte,
                            amountAsset: String,
                            priceAsset: String,
                            feeAsset: String,
                            amount: Double,
                            price: Double,
                            fee: Double)

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
          _.amountAsset     -> "amount_asset_id",
          _.priceAsset      -> "price_asset_id",
          _.feeAsset        -> "fee_asset_id",
          _.amount          -> "amount",
          _.price           -> "price",
          _.fee             -> "fee"
        ).filter(_.id == lift(orderId))
      )
      .headOption

  def getEventsInfoByOrderId(orderId: String): Set[EventBriefInfo] =
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
        ).filter(_.orderId == lift(orderId))
      )
      .toSet

  implicit class DoubleOps(value: Double) {
    val wct, usd: Long      = Normalization.normalizeAmountAndFee(value, Decimals)
    val eth, btc: Long      = Normalization.normalizeAmountAndFee(value, 8)
    val wctUsdPrice: Long   = Normalization.normalizePrice(value, Decimals, Decimals)
    val wavesUsdPrice: Long = Normalization.normalizePrice(value, 8, Decimals)
  }

  def stringify(asset: Asset): String = AssetPair.assetIdStr(asset)

  "Order history should save all orders and events" in {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { _ =>
        node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 0.35.wctUsdPrice, 0.003.waves)
        node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 0.35.wctUsdPrice, 0.003.waves)
      }

    retry(10, batchLingerMs) {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events: 1 big counter and 2 small submitted" in {

    def sellOrder: Order = node.prepareOrder(bob, wctUsdPair, SELL, 100.wct, 0.35.wctUsdPrice, fee = 0.00000030.btc, feeAsset = btc, version = 3)
    val buyOrder         = node.placeOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.wctUsdPrice, fee = 0.00001703.eth, feeAsset = eth, version = 3).message.id

    val sellOrder1 = node.placeOrder(sellOrder).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder1, "Filled")

    val sellOrder2 = node.placeOrder(sellOrder).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder2, "Filled")

    node.cancelOrder(alice, wctUsdPair, buyOrder)

    retry(10, batchLingerMs) {

      withClue("checking info for 2 small submitted orders\n") {

        Set(sellOrder1, sellOrder2).foreach { orderId =>
          getOrderInfoById(orderId).get shouldBe
            OrderBriefInfo(orderId,
                           limitOrderType,
                           bob.publicKey.toString,
                           sellSide,
                           stringify(wct),
                           stringify(usd),
                           stringify(btc),
                           100,
                           0.35,
                           0.00000030)

          getEventsInfoByOrderId(orderId) shouldBe Set { EventBriefInfo(orderId, eventTrade, 100, 100, 0.00000030, 0.00000030, statusFilled) }
        }
      }

      withClue("checking info for 1 big counter order\n") {
        getOrderInfoById(buyOrder).get shouldBe
          OrderBriefInfo(buyOrder,
                         limitOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         300,
                         0.35,
                         0.00001703)

        getEventsInfoByOrderId(buyOrder) shouldBe
          Set(
            EventBriefInfo(buyOrder, eventTrade, 100, 100, 0.00000567, 0.00000567, statusPartiallyFilled),
            EventBriefInfo(buyOrder, eventTrade, 100, 200, 0.00000567, 0.00001134, statusPartiallyFilled),
            EventBriefInfo(buyOrder, eventCancel, 0, 200, 0, 0.00001134, statusCancelled)
          )
      }
    }
  }

  "Order history should correctly save events with Waves as amount and fee" in {
    val buyOrder =
      node.placeOrder(alice, wavesUsdPair, BUY, 300.waves, 0.35.wavesUsdPrice, fee = 0.00370300.waves, feeAsset = Waves, version = 3).message.id
    val sellOrder = node.placeOrder(bob, wavesUsdPair, SELL, 300.waves, 0.35.wavesUsdPrice, fee = 0.30.usd, feeAsset = usd, version = 3).message.id

    node.waitOrderStatus(wavesUsdPair, buyOrder, "Filled")
    node.waitOrderStatus(wavesUsdPair, sellOrder, "Filled")

    retry(20, batchLingerMs) {
      withClue("checking info for counter order\n") {
        getOrderInfoById(buyOrder).get shouldBe OrderBriefInfo(buyOrder,
                                                               limitOrderType,
                                                               alice.publicKey.toString,
                                                               buySide,
                                                               "WAVES",
                                                               stringify(usd),
                                                               "WAVES",
                                                               300,
                                                               0.35,
                                                               0.00370300)
        getEventsInfoByOrderId(buyOrder) shouldBe Set {
          EventBriefInfo(buyOrder, eventTrade, 300, 300, 0.00370300, 0.00370300, statusFilled)
        }
      }

      withClue("checking info for submitted order\n") {
        getOrderInfoById(sellOrder).get shouldBe OrderBriefInfo(sellOrder,
                                                                limitOrderType,
                                                                bob.publicKey.toString,
                                                                sellSide,
                                                                "WAVES",
                                                                stringify(usd),
                                                                stringify(usd),
                                                                300,
                                                                0.35,
                                                                0.30)

        getEventsInfoByOrderId(sellOrder) shouldBe Set {
          EventBriefInfo(sellOrder, eventTrade, 300, 300, 0.30, 0.30, statusFilled)
        }
      }
    }
  }

  "Order history should correctly save events: 1 small counter and 1 big submitted" in {

    val smallBuyOrder =
      node.placeOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.wctUsdPrice, fee = 0.00001703.eth, feeAsset = eth, version = 3).message.id
    val bigSellOrder = node.placeOrder(bob, wctUsdPair, SELL, 900.wct, 0.35.wctUsdPrice, fee = 0.00000030.btc, feeAsset = btc, version = 3).message.id

    node.waitOrderStatus(wctUsdPair, smallBuyOrder, "Filled")
    node.waitOrderStatus(wctUsdPair, bigSellOrder, "PartiallyFilled")

    retry(20, batchLingerMs) {

      withClue("checking info for small counter order\n") {
        getOrderInfoById(smallBuyOrder).get shouldBe
          OrderBriefInfo(smallBuyOrder,
                         limitOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         300,
                         0.35,
                         0.00001703)

        getEventsInfoByOrderId(smallBuyOrder) shouldBe Set {
          EventBriefInfo(smallBuyOrder, eventTrade, 300, 300, 0.00001703, 0.00001703, statusFilled)
        }
      }

      withClue("checking info for big submitted order\n") {
        getOrderInfoById(bigSellOrder).get shouldBe
          OrderBriefInfo(bigSellOrder,
                         limitOrderType,
                         bob.publicKey.toString,
                         sellSide,
                         stringify(wct),
                         stringify(usd),
                         stringify(btc),
                         900,
                         0.35,
                         0.00000030)

        getEventsInfoByOrderId(bigSellOrder) shouldBe Set {
          EventBriefInfo(bigSellOrder, eventTrade, 300, 300, 0.00000010, 0.00000010, statusPartiallyFilled)
        }
      }
    }
    node.cancelAllOrders(alice)
    node.cancelAllOrders(bob)
  }

  "Order history should save fee info" in {
    val feeAsset = eth
    node.upsertRate(feeAsset, 0.005, expectedStatusCode = StatusCodes.OK)

    withClue("in placed and cancelled order") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee, version = 3).message.id
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe None
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
          node.ordersByAddress(alice, activeOnly),
          node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe 0
            orderbook.feeAsset shouldBe Waves
          }
        )
      }
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 0
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
        node.ordersByAddress(alice, activeOnly = true),
        node.activeOrderHistory(alice)).foreach(orderbookHistory =>
          orderbookHistory.find(_.id == aliceOrderId) shouldBe None
      )
    }

    withClue("in placed and cancelled orderV3") {
      val aliceOrderV3Id = node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe None
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
          node.ordersByAddress(alice, activeOnly),
          node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe 0
            orderbook.feeAsset shouldBe feeAsset
          }
        )
      }
      node.cancelOrder(alice, wctUsdPair, aliceOrderV3Id)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 0
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
        node.ordersByAddress(alice, activeOnly = true),
        node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in filled orders of different versions") {
      val aliceOrderV3Id = node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      val bobOrderId     = node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderV3Id)
      Array(bobOrderId, aliceOrderV3Id).foreach((id: String) => node.orderStatus(id, wctUsdPair).filledFee shouldBe Some(matcherFee))
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(bob, wctUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
        node.ordersByAddress(alice, activeOnly = true),
        node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in partially filled and cancelled orders") {
      val aliceOrderV3Id = node.placeOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee).message.id
      val bobOrderId     = node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
          node.ordersByAddress(alice, activeOnly),
          node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe matcherFee / 2
            orderbook.feeAsset shouldBe Waves
          }
        )
      }
      Array(node.orderHistoryByPair(bob, wctUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee / 2
          orderbook.feeAsset shouldBe Waves
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
        node.ordersByAddress(alice, activeOnly = true),
        node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in partially filled and cancelled orders of different versions") {
      val aliceOrderV3Id =
        node.placeOrder(alice, wctUsdPair, BUY, 2.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      val bobOrderId = node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      for (activeOnly <- Array(true, false)) {
        Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly),
          node.ordersByAddress(alice, activeOnly),
          node.fullOrderHistory(alice, Some(activeOnly))).foreach(
          orderbookHistory => {
            val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe matcherFee / 2
            orderbook.feeAsset shouldBe feeAsset
          }
        )
      }
      Array(node.orderHistoryByPair(bob, wctUsdPair), node.ordersByAddress(bob, activeOnly = false), node.fullOrderHistory(bob)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == bobOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe Waves
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderV3Id)
      node.orderStatus(aliceOrderV3Id, wctUsdPair).filledFee shouldBe Some(matcherFee / 2)
      node.orderStatus(bobOrderId, wctUsdPair).filledFee shouldBe Some(matcherFee)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderV3Id).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee / 2
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      Array(node.orderHistoryByPair(alice, wctUsdPair, activeOnly = true),
        node.ordersByAddress(alice, activeOnly = true),
        node.activeOrderHistory(alice)).foreach(
        orderbookHistory => orderbookHistory.find(_.id == aliceOrderV3Id) shouldBe None
      )
    }

    withClue("in partially filled orders with fractional filled amount") {
      val aliceOrderId = node.placeOrder(alice, wctUsdPair, BUY, 9.wct, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 1.price, matcherFee).message.id
      node.waitOrderInBlockchain(aliceOrderId)
      node.orderStatus(aliceOrderId, wctUsdPair).filledFee shouldBe Some(33333)
      Array(node.orderHistoryByPair(alice, wctUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe 33333
          orderbook.feeAsset shouldBe feeAsset
        }
      )
      node.cancelOrder(alice, wctUsdPair, aliceOrderId)
    }

    withClue("should should right fee if not enoght amount before order execution") {
      val ethBalance = node.tradableBalance(alice, ethUsdPair)(EthId.toString)
      val aliceSellOrderId =
        node.placeOrder(alice, ethUsdPair, SELL, ethBalance - matcherFee, 1.price, matcherFee, version = 3, feeAsset = feeAsset).message.id
      val aliceBuyOrderId =
        node.placeOrder(alice, ethUsdPair, BUY, 1.eth, 0.5.price, matcherFee, version = 3, feeAsset = feeAsset).message.id

      Array(node.orderHistoryByPair(alice, ethUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          for (orderId <- Array(aliceBuyOrderId, aliceSellOrderId)) {
            val orderbook = orderbookHistory.find(_.id == orderId).get
            orderbook.fee shouldBe matcherFee
            orderbook.filledFee shouldBe 0
            orderbook.feeAsset shouldBe feeAsset
          }
        }
      )
      node.placeOrder(bob, ethUsdPair, SELL, 1.eth, 0.5.price, matcherFee, version = 3).message.id
      node.waitOrderInBlockchain(aliceBuyOrderId)
      node.orderStatus(aliceBuyOrderId, ethUsdPair).filledFee shouldBe Some(matcherFee)
      Array(node.orderHistoryByPair(alice, ethUsdPair), node.ordersByAddress(alice, activeOnly = false), node.fullOrderHistory(alice)).foreach(
        orderbookHistory => {
          val orderbook = orderbookHistory.find(_.id == aliceBuyOrderId).get
          orderbook.fee shouldBe matcherFee
          orderbook.filledFee shouldBe matcherFee
          orderbook.feeAsset shouldBe feeAsset
        }
      )
    }
  }

  "Order history should correctly save market orders and their events" in {

    node.cancelAllOrders(bob)
    node.cancelAllOrders(alice)

    def bigBuyOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, 500.wct, 0.35.wctUsdPrice, fee = 0.00001703.eth, feeAsset = eth, version = 3)

    withClue("place buy market order into empty order book") {

      val unmatchableMarketBuyOrder = node.placeMarketOrder(bigBuyOrder).message.id
      node.waitOrderStatusAndAmount(wctUsdPair, unmatchableMarketBuyOrder, "Filled", Some(0.wct))

      retry(20, batchLingerMs) {

        getOrderInfoById(unmatchableMarketBuyOrder) shouldBe Some(
          OrderBriefInfo(unmatchableMarketBuyOrder,
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

        getEventsInfoByOrderId(unmatchableMarketBuyOrder) shouldBe Set(
          EventBriefInfo(unmatchableMarketBuyOrder, eventCancel, 0, 0, 0, 0, statusFilled)
        )
      }
    }

    withClue("place buy market order into nonempty order book") {
      Seq(
        node.placeOrder(bob, wctUsdPair, SELL, 100.wct, 0.33.wctUsdPrice, 0.003.waves).message.id,
        node.placeOrder(bob, wctUsdPair, SELL, 100.wct, 0.34.wctUsdPrice, 0.003.waves).message.id,
        node.placeOrder(bob, wctUsdPair, SELL, 100.wct, 0.34.wctUsdPrice, 0.003.waves).message.id
      ).foreach(lo => node.waitOrderStatus(wctUsdPair, lo, "Accepted"))

      val marketBuyOrder = node.placeMarketOrder(bigBuyOrder).message.id
      node.waitOrderStatusAndAmount(wctUsdPair, marketBuyOrder, "Filled", Some(300.wct))

      retry(15, batchLingerMs) {

        getOrderInfoById(marketBuyOrder).get shouldBe
          OrderBriefInfo(marketBuyOrder,
                         marketOrderType,
                         alice.publicKey.toString,
                         buySide,
                         stringify(wct),
                         stringify(usd),
                         stringify(eth),
                         500,
                         0.35,
                         0.00001703)

        getEventsInfoByOrderId(marketBuyOrder) shouldBe
          Set(
            EventBriefInfo(marketBuyOrder, eventTrade, 100, 100, 0.00000340, 0.00000340, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder, eventTrade, 100, 200, 0.00000340, 0.00000680, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder, eventTrade, 100, 300, 0.00000340, 0.00001020, statusPartiallyFilled),
            EventBriefInfo(marketBuyOrder, eventCancel, 0, 300, 0, 0.00001020, statusFilled)
          )
      }
    }
  }
}
