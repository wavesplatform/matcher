package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.responses.dex.{OrderStatus => ApiOrderStatus}
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WebSocketConnection}
import com.wavesplatform.it.MatcherSuiteBase

import scala.collection.immutable.TreeMap

class WebSocketPublicStreamTestSuite extends MatcherSuiteBase with HasWebSockets {

  private val carol = mkKeyPair("carol")

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, carol, 100.waves, Waves), mkTransfer(bob, carol, 1.btc, btc))
    dex1.start()
    dex1.api.upsertRate(btc, 0.00011167)
  }

  private def receiveAtLeastN[T](wsc: WebSocketConnection[T], n: Int): Seq[T] = {
    eventually { wsc.getMessagesBuffer.size should be >= n }
    Thread.sleep(200) // Waiting for additional messages
    wsc.getMessagesBuffer
  }

  "Connection should be established" in {
    val wsc = mkWebSocketAuthenticatedConnection(alice, dex1)
    wsc.close()
    wsc.getMessagesBuffer.foreach { x =>
      x.balances should not be empty
    }
  }

  "MatcherWebSocketRoute" - {
    "orderbook" - {
      "should send a full state after connection" in {
        // Force create an order book to pass a validation in the route
        val firstOrder = mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403)
        placeAndAwaitAtDex(firstOrder)
        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(firstOrder, ApiOrderStatus.Cancelled)

        markup("No orders")
        val wsc0    = mkWebSocketOrderBookConnection(wavesBtcPair, dex1)
        val buffer0 = receiveAtLeastN(wsc0, 1)
        wsc0.close()

        buffer0 should have size 1
        squashOrderBooks(buffer0) should matchTo(
          WsOrderBook(
            asks = TreeMap.empty,
            bids = TreeMap.empty,
            lastTrade = None,
            timestamp = buffer0.last.timestamp
          )
        )

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403))

        markup("One order")

        val wsc1    = mkWebSocketOrderBookConnection(wavesBtcPair, dex1)
        val buffer1 = receiveAtLeastN(wsc1, 1)
        wsc1.close()

        buffer1 should have size 1
        squashOrderBooks(buffer1) should matchTo(
          WsOrderBook(
            asks = TreeMap.empty,
            bids = TreeMap(0.00011403d -> 1.05d),
            lastTrade = None
          )
        )

        markup("Two orders")

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00012))

        val wsc2    = mkWebSocketOrderBookConnection(wavesBtcPair, dex1)
        val buffer2 = receiveAtLeastN(wsc2, 1)
        wsc2.close()

        buffer2 should have size 1
        squashOrderBooks(buffer2) should matchTo(
          WsOrderBook(
            asks = TreeMap(0.00012d    -> 1d),
            bids = TreeMap(0.00011403d -> 1.05d),
            lastTrade = None
          )
        )

        markup("Two orders and trade")

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 0.5.waves, 0.00013), ApiOrderStatus.Filled)

        val wsc3    = mkWebSocketOrderBookConnection(wavesBtcPair, dex1)
        val buffer3 = receiveAtLeastN(wsc3, 1)
        wsc3.close()

        buffer3.size should (be >= 1 and be <= 2)
        squashOrderBooks(buffer3) should matchTo(
          WsOrderBook(
            asks = TreeMap(0.00012d    -> 0.5d),
            bids = TreeMap(0.00011403d -> 1.05d),
            lastTrade = WsLastTrade(
              price = 0.00012d,
              amount = 0.5,
              side = OrderType.BUY
            ).some
          )
        )

        markup("Four orders")

        List(
          mkOrderDP(carol, wavesBtcPair, SELL, 0.6.waves, 0.00013),
          mkOrderDP(carol, wavesBtcPair, BUY, 0.7.waves, 0.000115)
        ).foreach(placeAndAwaitAtDex(_))

        val wsc4    = mkWebSocketOrderBookConnection(wavesBtcPair, dex1)
        val buffer4 = receiveAtLeastN(wsc4, 1)
        wsc4.close()

        buffer4.size should (be >= 1 and be <= 2)
        // TODO this test won't check ordering :(
        squashOrderBooks(buffer4) should matchTo(
          WsOrderBook(
            asks = TreeMap(
              0.00012d -> 0.5d,
              0.00013d -> 0.6d,
            ),
            bids = TreeMap(
              0.000115d   -> 0.7d,
              0.00011403d -> 1.05d
            ),
            lastTrade = WsLastTrade(
              price = 0.00012d,
              amount = 0.5,
              side = OrderType.BUY
            ).some
          )
        )

        dex1.api.cancelAll(carol)
      }

      "should send updates" in {
        val firstOrder = mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403)
        placeAndAwaitAtDex(firstOrder)
        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(firstOrder, ApiOrderStatus.Cancelled)

        val wsc = mkWebSocketOrderBookConnection(wavesBtcPair, dex1)
        receiveAtLeastN(wsc, 1)
        wsc.clearMessagesBuffer()

        markup("A new order")
        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00012))

        val buffer1 = receiveAtLeastN(wsc, 1)
        buffer1 should have size 1
        squashOrderBooks(buffer1) should matchTo(
          WsOrderBook(
            asks = TreeMap.empty,
            bids = TreeMap(0.00012d -> 1d),
            lastTrade = None
          )
        )
        wsc.clearMessagesBuffer()

        markup("An execution and adding a new order")
        val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.5.waves, 0.00012)
        placeAndAwaitAtDex(order, ApiOrderStatus.PartiallyFilled)

        val buffer2 = receiveAtLeastN(wsc, 1)
        buffer2.size should (be >= 1 and be <= 2)
        squashOrderBooks(buffer2) should matchTo(
          WsOrderBook(
            asks = TreeMap(0.00012d -> 0.5d),
            bids = TreeMap(0.00012d -> 0d),
            lastTrade = WsLastTrade(
              price = 0.00012d,
              amount = 1,
              side = OrderType.SELL
            ).some
          )
        )
        wsc.clearMessagesBuffer()

        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(order, ApiOrderStatus.Cancelled)

        val buffer3 = receiveAtLeastN(wsc, 1)
        buffer3.size shouldBe 1
        squashOrderBooks(buffer3) should matchTo(
          WsOrderBook(
            asks = TreeMap(0.00012d -> 0d),
            bids = TreeMap.empty,
            lastTrade = None
          )
        )
        wsc.clearMessagesBuffer()
      }
    }
  }
}