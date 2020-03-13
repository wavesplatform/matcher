package com.wavesplatform.it.sync.api

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsLastTrade, WsOrder, WsOrderBook}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.api.responses.dex.{OrderStatus => ApiOrderStatus}
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WebSocketAuthenticatedConnection, WebSocketConnection}
import com.wavesplatform.dex.model.{LimitOrder, OrderStatus}
import com.wavesplatform.it.MatcherSuiteBase
import play.api.libs.json.Json

import scala.collection.immutable.TreeMap

class MatcherWebSocketsTestSuite extends MatcherSuiteBase with HasWebSockets {

  private implicit val efc: ErrorFormatterContext = assetDecimalsMap.apply

  private val carol = mkKeyPair("carol")

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, carol, 100.waves, Waves), mkTransfer(bob, carol, 1.btc, btc))
    dex1.start()
    dex1.api.upsertRate(btc, 0.00011167)
  }

  private def squashBalanceChanges(xs: Seq[Map[Asset, WsBalances]]): Map[Asset, WsBalances] = xs.foldLeft(Map.empty[Asset, WsBalances]) { _ ++ _ }

  private def assertAddressStateSnapshot(connection: WebSocketAuthenticatedConnection, expectedSnapshot: WsAddressState): Unit = {
    eventually { connection.getMessagesBuffer should have size 1 }
    Thread.sleep(200)
    connection.getSnapshot should matchTo(expectedSnapshot)
    connection.clearMessagesBuffer()
  }

  private def assertAddressStateChanges(connection: WebSocketAuthenticatedConnection,
                                        balancesChangesCountBorders: (Int, Int),
                                        ordersChangesCount: Int,
                                        expectedBalanceChanges: Map[Asset, WsBalances],
                                        expectedOrdersChanges: Seq[WsOrder]): Unit = {
    eventually {
      connection.getBalancesChanges.size should be >= balancesChangesCountBorders._1
      connection.getOrderChanges.size shouldEqual ordersChangesCount
    }

    Thread.sleep(200)

    connection.getBalancesChanges.size should be <= balancesChangesCountBorders._2

    squashBalanceChanges(connection.getBalancesChanges) should matchTo(expectedBalanceChanges)
    connection.getOrderChanges should matchTo(expectedOrdersChanges)

    connection.clearMessagesBuffer()
  }

  private def createOrderBookWsConnection(assetPair: AssetPair): WebSocketConnection[WsOrderBook] = {
    val wsUri = s"127.0.0.1:${dex1.restApiAddress.getPort}/ws/orderbook/${assetPair.amountAssetStr}/${assetPair.priceAssetStr}"
    mkWebSocketConnection(wsUri) { msg =>
      val text = msg.asTextMessage.getStrictText
      log.info(s"Got message: $text")
      Json.parse(text).as[WsOrderBook]
    }
  }

  "Connection should be established" in {
    val wsc = mkWebSocketAuthenticatedConnection(alice, dex1)
    wsc.close()
    wsc.getMessagesBuffer.foreach { x =>
      x.balances should not be empty
    }
  }

  "MatcherWebSocketRoute" - {

    "should send account updates to authenticated user" - {

      "when account is empty" in {
        val wsac = mkWebSocketAuthenticatedConnection(mkKeyPair("JIo6cTep_u3_6ocToHa"), dex1)
        assertAddressStateSnapshot(wsac, WsAddressState.empty)
        wsac.close()
      }

      "when sender places and cancels orders" in {

        // Carol has 100 Waves and 1 BTC
        val wsac = mkWebSocketAuthenticatedConnection(carol, dex1)

        assertAddressStateSnapshot(
          connection = wsac,
          expectedSnapshot = WsAddressState(
            balances = Map(Waves -> WsBalances(100, 0), btc -> WsBalances(1, 0)),
            orders = Seq.empty
          )
        )

        val buyOrder  = mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00011403)
        val sellOrder = mkOrderDP(carol, wavesUsdPair, SELL, 1.waves, 3.01)

        placeAndAwaitAtDex(buyOrder)
        placeAndAwaitAtDex(sellOrder)

        Seq(buyOrder, sellOrder).foreach { o =>
          dex1.api.cancel(carol, o)
          dex1.api.waitForOrderStatus(o, ApiOrderStatus.Cancelled)
        }

        assertAddressStateChanges(
          connection = wsac,
          balancesChangesCountBorders = (2, 4),
          ordersChangesCount = 4,
          expectedBalanceChanges = squashBalanceChanges(
            Seq(
              Map(btc   -> WsBalances(tradable = 0.99988597, reserved = 0.00011403)), // reserve
              Map(Waves -> WsBalances(tradable = 98.997, reserved = 1.003)), // reserve
              Map(btc   -> WsBalances(tradable = 1, reserved = 0)), // cancel
              Map(Waves -> WsBalances(tradable = 100, reserved = 0)) // cancel
            )
          ),
          expectedOrdersChanges = Seq(
            WsOrder.fromDomain(LimitOrder(buyOrder), OrderStatus.Accepted),
            WsOrder.fromDomain(LimitOrder(sellOrder), OrderStatus.Accepted),
            WsOrder(buyOrder.id(), status = OrderStatus.Cancelled.name.some),
            WsOrder(sellOrder.id(), status = OrderStatus.Cancelled.name.some)
          )
        )

        wsac.close()
      }

      "when sender's order executes" in {

        broadcastAndAwait(mkTransfer(alice, carol, 200.usd, usd))

        // Carol has 100 Waves, 1 BTC and 200 USD
        val wsac = mkWebSocketAuthenticatedConnection(carol, dex1)

        assertAddressStateSnapshot(
          connection = wsac,
          expectedSnapshot = WsAddressState(
            balances = Map(Waves -> WsBalances(100, 0), btc -> WsBalances(1, 0), usd -> WsBalances(200, 0)),
            orders = Seq.empty
          )
        )

        placeAndAwaitAtDex(mkOrderDP(bob, wavesUsdPair, SELL, 6.waves, 3.0))

        val buyOrder = mkOrderDP(carol, wavesUsdPair, BUY, 12.waves, 3.0, 0.00000034.btc, btc)

        placeAndAwaitAtNode(buyOrder)

        dex1.api.cancel(carol, buyOrder)
        dex1.api.waitForOrderStatus(buyOrder, ApiOrderStatus.Cancelled)

        assertAddressStateChanges(
          connection = wsac,
          balancesChangesCountBorders = (3, 8),
          ordersChangesCount = 2,
          expectedBalanceChanges = squashBalanceChanges(
            Seq(
              Map(usd   -> WsBalances(200, 0)), // transfer
              Map(usd   -> WsBalances(164, 36), btc -> WsBalances(0.99999966, 0.00000034)), // reserve
              Map(usd   -> WsBalances(164, 18), btc -> WsBalances(0.99999966, 0.00000017)), // execution
              Map(Waves -> WsBalances(106, 0)), // execution
              Map(usd   -> WsBalances(182, 0), btc -> WsBalances(0.99999983, 0)) // cancelling
            )
          ),
          expectedOrdersChanges = Seq(
            WsOrder
              .fromDomain(LimitOrder(buyOrder), OrderStatus.PartiallyFilled(6.waves, 0.00000017.btc))
              .copy(
                filledAmount = 6.0.some,
                filledFee = 0.00000017.some,
                avgFilledPrice = 3.0.some
              ),
            WsOrder(buyOrder.id(), status = OrderStatus.Cancelled.name.some)
          )
        )

        wsac.close()
      }
    }

    "orderbook" - {
      "should send a full state after connection" in {
        // Force create an order book to pass a validation in the route
        val firstOrder = mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403)
        placeAndAwaitAtDex(firstOrder)
        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(firstOrder, ApiOrderStatus.Cancelled)

        markup("No orders")
        val wsc0    = createOrderBookWsConnection(wavesBtcPair)
        val buffer0 = receiveAtLeastN(wsc0, 1)
        wsc0.close()

        buffer0 should have size 1
        squash(buffer0) shouldBe WsOrderBook(
          asks = TreeMap.empty,
          bids = TreeMap.empty,
          lastTrade = None
        )

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403))

        markup("One order")

        val wsc1    = createOrderBookWsConnection(wavesBtcPair)
        val buffer1 = receiveAtLeastN(wsc1, 1)
        wsc1.close()

        buffer1 should have size 1
        squash(buffer1) shouldBe WsOrderBook(
          asks = TreeMap.empty,
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = None
        )

        markup("Two orders")

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00012))

        val wsc2    = createOrderBookWsConnection(wavesBtcPair)
        val buffer2 = receiveAtLeastN(wsc2, 1)
        wsc2.close()

        buffer2 should have size 1
        squash(buffer2) shouldBe WsOrderBook(
          asks = TreeMap(0.00012d    -> 1d),
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = None
        )

        markup("Two orders and trade")

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 0.5.waves, 0.00013), ApiOrderStatus.Filled)

        val wsc3    = createOrderBookWsConnection(wavesBtcPair)
        val buffer3 = receiveAtLeastN(wsc3, 1)
        wsc3.close()

        buffer3.size should (be >= 1 and be <= 2)
        squash(buffer3) shouldBe WsOrderBook(
          asks = TreeMap(0.00012d    -> 0.5d),
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = Some(
            WsLastTrade(
              price = 0.00012d,
              amount = 0.5,
              side = OrderType.BUY
            ))
        )

        markup("Four orders")

        List(
          mkOrderDP(carol, wavesBtcPair, SELL, 0.6.waves, 0.00013),
          mkOrderDP(carol, wavesBtcPair, BUY, 0.7.waves, 0.000115)
        ).foreach(placeAndAwaitAtDex(_))

        val wsc4    = createOrderBookWsConnection(wavesBtcPair)
        val buffer4 = receiveAtLeastN(wsc4, 1)
        wsc4.close()

        buffer4.size should (be >= 1 and be <= 2)
        // TODO this test won't check ordering :(
        squash(buffer4) shouldBe WsOrderBook(
          asks = TreeMap(
            0.00012d -> 0.5d,
            0.00013d -> 0.6d,
          ),
          bids = TreeMap(
            0.000115d   -> 0.7d,
            0.00011403d -> 1.05d
          ),
          lastTrade = Some(
            WsLastTrade(
              price = 0.00012d,
              amount = 0.5,
              side = OrderType.BUY
            ))
        )

        dex1.api.cancelAll(carol)
      }

      "should send updates" in {
        val firstOrder = mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403)
        placeAndAwaitAtDex(firstOrder)
        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(firstOrder, ApiOrderStatus.Cancelled)

        val wsc = createOrderBookWsConnection(wavesBtcPair)
        receiveAtLeastN(wsc, 1)
        wsc.clearMessagesBuffer()

        markup("A new order")
        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00012))

        val buffer1 = receiveAtLeastN(wsc, 1)
        buffer1 should have size 1
        squash(buffer1) shouldBe WsOrderBook(
          asks = TreeMap.empty,
          bids = TreeMap(0.00012d -> 1d),
          lastTrade = None
        )
        wsc.clearMessagesBuffer()

        markup("An execution and adding a new order")
        val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.5.waves, 0.00012)
        placeAndAwaitAtDex(order, ApiOrderStatus.PartiallyFilled)

        val buffer2 = receiveAtLeastN(wsc, 1)
        buffer2.size should (be >= 1 and be <= 2)
        squash(buffer2) shouldBe WsOrderBook(
          asks = TreeMap(0.00012d -> 0.5d),
          bids = TreeMap(0.00012d -> 0d),
          lastTrade = Some(
            WsLastTrade(
              price = 0.00012d,
              amount = 1,
              side = OrderType.SELL
            ))
        )
        wsc.clearMessagesBuffer()

        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(order, ApiOrderStatus.Cancelled)

        val buffer3 = receiveAtLeastN(wsc, 1)
        buffer3.size shouldBe 1
        squash(buffer3) shouldBe WsOrderBook(
          asks = TreeMap(0.00012d -> 0d),
          bids = TreeMap.empty,
          lastTrade = None
        )
        wsc.clearMessagesBuffer()
      }
    }
  }

  private def squash(xs: TraversableOnce[WsOrderBook]): WsOrderBook = xs.foldLeft(WsOrderBook.empty) {
    case (r, x) =>
      WsOrderBook(
        asks = r.asks ++ x.asks,
        bids = r.bids ++ x.bids,
        lastTrade = r.lastTrade.orElse(x.lastTrade)
      )
  }

  private def receiveAtLeastN[T](wsc: WebSocketConnection[T], n: Int): Seq[T] = {
    eventually {
      wsc.getMessagesBuffer.size should be >= n
    }
    Thread.sleep(200) // Waiting for additional messages
    wsc.getMessagesBuffer
  }

}
