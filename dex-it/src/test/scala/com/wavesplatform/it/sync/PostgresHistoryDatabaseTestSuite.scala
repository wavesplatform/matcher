package com.wavesplatform.it.sync

import java.net.InetAddress
import java.sql.{Connection, DriverManager}

import akka.http.scaladsl.model.StatusCodes
import com.google.common.primitives.Ints
import com.spotify.docker.client.messages.Network
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.dex.history.HistoryRouter._
import com.wavesplatform.dex.model.OrderValidator
import com.wavesplatform.dex.settings.PostgresConnection._
import com.wavesplatform.dex.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.{DockerContainerLauncher, MatcherSuiteBase}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import io.getquill.{PostgresJdbcContext, SnakeCase}
import net.ceedubs.ficus.Ficus._

import scala.io.Source
import scala.util.{Failure, Try}

class PostgresHistoryDatabaseTestSuite extends MatcherSuiteBase {

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
      List(postgresEnv),
      postgresContainerIp,
      postgresContainerPort,
      wavesNetwork.name
    )

  val batchLingerMs: Int = OrderHistorySettings.defaultBatchLingerMs

  def getPostgresContainerHostPort: String = postgresContainerLauncher.getHostPort.explicitGet()

  @annotation.tailrec
  private def retry[T](attemptsCount: Int, delayInMs: Int = 0)(fn: => T): Try[T] = {
    Try { fn } match {
      case Failure(_) if attemptsCount > 1 => if (delayInMs != 0) Thread.sleep(delayInMs); retry(attemptsCount - 1)(fn)
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

    Seq(IssueUsdTx, IssueWctTx, IssueEthTx, IssueBtcTx).foreach { tx =>
      node.waitForTransaction { node.broadcastRequest(tx.json.value).id }
    }

    node.upsertRate(eth, 0.00567593, expectedStatusCode = StatusCodes.Created)
    node.upsertRate(btc, 0.00009855, expectedStatusCode = StatusCodes.Created)
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

  def stringify(asset: Asset): String = AssetPair.assetIdStr(asset)

  "Order history should save all orders and events" in {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { _ =>
        node.placeOrder(alice, wctUsdPair, BUY, 1.wct, 0.35.price, 0.003.waves)
        node.placeOrder(bob, wctUsdPair, SELL, 1.wct, 0.35.price, 0.003.waves)
      }

    retry(10, batchLingerMs) {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events: 1 big counter and 2 small submitted" in {

    def sellOrder: Order = node.prepareOrder(bob, wctUsdPair, SELL, 100.wct, 0.35.price, fee = 0.00000030.btc, feeAssetId = btc, version = 3)
    val buyOrder         = node.placeOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.price, fee = 0.00001703.eth, feeAsset = eth, version = 3).message.id

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
            OrderBriefInfo(orderId, bob.publicKey.toString, sellSide, stringify(wct), stringify(usd), stringify(btc), 100, 0.35, 0.00000030)

          getEventsInfoByOrderId(orderId) shouldBe Set { EventBriefInfo(orderId, eventTrade, 100, 100, 0.00000030, 0.00000030, statusFilled) }
        }
      }

      withClue("checking info for 1 big counter order\n") {
        getOrderInfoById(buyOrder).get shouldBe
          OrderBriefInfo(buyOrder, alice.publicKey.toString, buySide, stringify(wct), stringify(usd), stringify(eth), 300, 0.35, 0.00001703)

        getEventsInfoByOrderId(buyOrder) shouldBe
          Set(
            EventBriefInfo(buyOrder, eventTrade, 100, 100, 0.00000567, 0.00000567, statusPartiallyFilled),
            EventBriefInfo(buyOrder, eventTrade, 100, 200, 0.00000567, 0.00001134, statusPartiallyFilled),
            EventBriefInfo(buyOrder, eventCancel, 0, 200, 0, 0.00001134, statusCancelled)
          )
      }
    }
  }

  "Order history should correctly save events: 1 small counter and 1 big submitted" in {

    val smallBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, 300.wct, 0.35.price, fee = 0.00001703.eth, feeAsset = eth, version = 3).message.id
    val bigSellOrder  = node.placeOrder(bob, wctUsdPair, SELL, 900.wct, 0.35.price, fee = 0.00000030.btc, feeAsset = btc, version = 3).message.id

    node.waitOrderStatus(wctUsdPair, smallBuyOrder, "Filled")
    node.waitOrderStatus(wctUsdPair, bigSellOrder, "PartiallyFilled")

    retry(20, batchLingerMs) {

      withClue("checking info for small counter order\n") {
        getOrderInfoById(smallBuyOrder).get shouldBe
          OrderBriefInfo(smallBuyOrder, alice.publicKey.toString, buySide, stringify(wct), stringify(usd), stringify(eth), 300, 0.35, 0.00001703)

        getEventsInfoByOrderId(smallBuyOrder) shouldBe Set {
          EventBriefInfo(smallBuyOrder, eventTrade, 300, 300, 0.00001703, 0.00001703, statusFilled)
        }
      }

      withClue("checking info for big submitted order\n") {
        getOrderInfoById(bigSellOrder).get shouldBe
          OrderBriefInfo(bigSellOrder, bob.publicKey.toString, sellSide, stringify(wct), stringify(usd), stringify(btc), 900, 0.35, 0.00000030)

        getEventsInfoByOrderId(bigSellOrder) shouldBe Set {
          EventBriefInfo(bigSellOrder, eventTrade, 300, 300, 0.00000010, 0.00000010, statusPartiallyFilled)
        }
      }
    }
    node.cancelAllOrders(alice)
    node.cancelAllOrders(bob)
  }
}
