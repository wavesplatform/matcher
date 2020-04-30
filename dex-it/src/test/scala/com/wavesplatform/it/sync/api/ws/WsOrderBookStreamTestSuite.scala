package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.responses.dex.{OrderStatus => ApiOrderStatus}
import com.wavesplatform.dex.it.api.websockets.WsConnection
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.it.WsSuiteBase

import scala.collection.immutable.TreeMap
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class WsOrderBookStreamTestSuite extends WsSuiteBase {

  private val carol = mkKeyPair("carol")

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, carol, 100.waves, Waves), mkTransfer(bob, carol, 1.btc, btc))
    dex1.start()
    dex1.api.upsertRate(btc, 0.00011167)
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
        val wsc0    = mkWsOrderBookConnection(wavesBtcPair, dex1)
        val buffer0 = wsc0.receiveAtLeastN[WsOrderBook](1)
        wsc0.close()

        buffer0 should have size 1
        buffer0.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap.empty,
            bids = TreeMap.empty,
            lastTrade = None,
            updateId = 0,
            timestamp = buffer0.last.timestamp
          )
        )

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403))

        markup("One order")

        val wsc1    = mkWsOrderBookConnection(wavesBtcPair, dex1)
        val buffer1 = wsc1.receiveAtLeastN[WsOrderBook](1)
        wsc1.close()

        buffer1 should have size 1
        buffer1.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap.empty,
            bids = TreeMap(0.00011403d -> 1.05d),
            lastTrade = None,
            updateId = 0,
            timestamp = buffer1.last.timestamp
          )
        )

        markup("Two orders")

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00012))

        val wsc2    = mkWsOrderBookConnection(wavesBtcPair, dex1)
        val buffer2 = wsc2.receiveAtLeastN[WsOrderBook](1)
        wsc2.close()

        buffer2 should have size 1
        buffer2.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap(0.00012d    -> 1d),
            bids = TreeMap(0.00011403d -> 1.05d),
            lastTrade = None,
            updateId = 0,
            timestamp = buffer2.last.timestamp
          )
        )

        markup("Two orders and trade")

        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 0.5.waves, 0.00013), ApiOrderStatus.Filled)

        val wsc3    = mkWsOrderBookConnection(wavesBtcPair, dex1)
        val buffer3 = wsc3.receiveAtLeastN[WsOrderBook](1)
        wsc3.close()

        buffer3.size should (be >= 1 and be <= 2)
        buffer3.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap(0.00012d    -> 0.5d),
            bids = TreeMap(0.00011403d -> 1.05d),
            lastTrade = WsLastTrade(
              price = 0.00012d,
              amount = 0.5,
              side = OrderType.BUY
            ).some,
            updateId = 0,
            timestamp = buffer3.last.timestamp
          )
        )

        markup("Four orders")

        List(
          mkOrderDP(carol, wavesBtcPair, SELL, 0.6.waves, 0.00013),
          mkOrderDP(carol, wavesBtcPair, BUY, 0.7.waves, 0.000115)
        ).foreach(placeAndAwaitAtDex(_))

        val wsc4    = mkWsOrderBookConnection(wavesBtcPair, dex1)
        val buffer4 = wsc4.receiveAtLeastN[WsOrderBook](1)
        wsc4.close()

        buffer4.size should (be >= 1 and be <= 2)
        // TODO this test won't check ordering :(
        buffer4.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
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
            ).some,
            updateId = buffer4.last.updateId,
            timestamp = buffer4.last.timestamp
          )
        )

        dex1.api.cancelAll(carol)
        Seq(wsc0, wsc1, wsc2, wsc3, wsc4).foreach { _.close() }
      }

      "should send updates" in {
        val wsc = mkWsOrderBookConnection(wavesBtcPair, dex1)
        wsc.receiveAtLeastN[WsOrderBook](1)
        wsc.clearMessages()

        markup("A new order")
        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00012))

        val buffer1 = wsc.receiveAtLeastN[WsOrderBook](1)
        buffer1 should have size 1
        buffer1.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap.empty,
            bids = TreeMap(0.00012d -> 1d),
            lastTrade = None,
            updateId = 1,
            timestamp = buffer1.last.timestamp
          )
        )
        wsc.clearMessages()

        markup("An execution and adding a new order")
        val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.5.waves, 0.00012)
        placeAndAwaitAtDex(order, ApiOrderStatus.PartiallyFilled)

        val buffer2 = wsc.receiveAtLeastN[WsOrderBook](1)
        buffer2.size should (be >= 1 and be <= 2)
        buffer2.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap(0.00012d -> 0.5d),
            bids = TreeMap(0.00012d -> 0d),
            lastTrade = WsLastTrade(
              price = 0.00012d,
              amount = 1,
              side = OrderType.SELL
            ).some,
            updateId = buffer2.last.updateId,
            timestamp = buffer2.last.timestamp
          )
        )
        wsc.clearMessages()

        dex1.api.cancelAll(carol)
        dex1.api.waitForOrderStatus(order, ApiOrderStatus.Cancelled)

        val buffer3 = wsc.receiveAtLeastN[WsOrderBook](1)
        buffer3.size shouldBe 1
        buffer3.squashed.values.head should matchTo(
          WsOrderBook(
            assetPair = wavesBtcPair,
            asks = TreeMap(0.00012d -> 0d),
            bids = TreeMap.empty,
            lastTrade = None,
            updateId = buffer3.last.updateId,
            timestamp = buffer3.last.timestamp
          )
        )

        wsc.clearMessages()
        wsc.close()
      }

      "should send correct update ids" in {

        def assertUpdateId(connection: WsConnection, expectedUpdateId: Long): Unit = {
          val buffer = connection.receiveAtLeastN[WsOrderBook](1)
          buffer should have size 1
          buffer.head.updateId shouldBe expectedUpdateId
          connection.clearMessages()
        }

        val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00005)

        val wsc1 = mkWsOrderBookConnection(wavesBtcPair, dex1)
        assertUpdateId(wsc1, 0)

        placeAndAwaitAtDex(order)
        assertUpdateId(wsc1, 1)

        val wsc2 = mkWsOrderBookConnection(wavesBtcPair, dex1)
        assertUpdateId(wsc2, 0)

        dex1.api.cancel(carol, order)
        assertUpdateId(wsc1, 2)
        assertUpdateId(wsc2, 1)
      }

      "should handle many connections simultaneously" in {
        Await
          .result(
            Future.traverse { (1 to 1000).toList }(
              _ =>
                Future {
                  val c = mkWsOrderBookConnection(wavesBtcPair, dex1)
                  Thread.sleep(200)
                  c.receiveAtLeastN[WsOrderBook](1).head.updateId
              }
            ),
            25.seconds
          ) forall (_ == 0) shouldBe true

        knownWsConnections.forEach { _.close() }
      }

      "should close connections when it is deleted" in {

        val seller                          = mkAccountWithBalance(100.waves -> Waves)
        val IssueResults(issueTx, _, asset) = mkIssueExtended(seller, "cJIoHoxpeH", 1000.asset8)
        val assetPair                       = AssetPair(asset, Waves)

        broadcastAndAwait(issueTx)
        dex1.api.place(mkOrderDP(seller, assetPair, SELL, 100.asset8, 5.0))

        val wsc1, wsc2, wsc3 = mkWsOrderBookConnection(assetPair, dex1)
        Seq(wsc1, wsc2, wsc3).foreach { _.receiveAtLeastN[WsOrderBook](1) }

        dex1.api.tryDeleteOrderBook(assetPair)

        eventually { Seq(wsc1, wsc2, wsc3).foreach(_.isClosed shouldBe true) }
      }
    }
  }
}
