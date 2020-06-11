package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.ApiOrderStatus
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.{WsLastTrade, WsOrderBookSettings}
import com.wavesplatform.dex.api.ws.protocol
import com.wavesplatform.dex.api.ws.protocol.{WsError, WsOrderBookChanges, WsOrderBookSubscribe, WsUnsubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.SubscriptionsLimitReached
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, OrderRestrictionsSettings}
import com.wavesplatform.it.WsSuiteBase

import scala.collection.immutable.TreeMap

class WsOrderBookStreamTestSuite extends WsSuiteBase {

  private val carol: KeyPair = mkKeyPair("carol")

  private val orderBookSettings: Option[WsOrderBookSettings] = WsOrderBookSettings(
    OrderRestrictionsSettings(
      stepAmount = 0.00000001,
      minAmount = 0.0000001,
      maxAmount = 200000000,
      stepPrice = 0.00000001,
      minPrice = 0.0000002,
      maxPrice = 300000
    ).some,
    DenormalizedMatchingRule.DefaultTickSize.toDouble.some
  ).some

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES", "$EthId" ]
       |  order-restrictions = {
       |    "WAVES-$BtcId": {
       |      step-amount = 0.00000001
       |      min-amount  = 0.0000001
       |      max-amount  = 200000000
       |      step-price  = 0.00000001
       |      min-price   = 0.0000002
       |      max-price   = 300000
       |    }
       |  }
       |  matching-rules = {
       |    "$EthId-WAVES": [
       |      {
       |        start-offset = 0
       |        tick-size    = 0.0002
       |      },
       |      {
       |        start-offset = 1
       |        tick-size    = 0.00000001
       |      }
       |    ]
       |  }
       |  web-sockets.web-socket-handler.subscriptions.max-order-book-number = 3
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx, IssueEthTx)
    broadcastAndAwait(mkTransfer(alice, carol, 100.waves, Waves), mkTransfer(bob, carol, 1.btc, btc))
    dex1.start()
    dex1.api.upsertRate(btc, 0.00011167)
  }

  "Order book stream should" - {
    "correctly handle rejections" in {
      val invalidAssetPair = AssetPair(Waves, eth)

      val wsc = mkWsConnection(dex1)
      wsc.send(WsOrderBookSubscribe(invalidAssetPair, 1))

      wsc.receiveAtLeastN[WsError](1).head should matchTo(
        WsError(
          timestamp = 0L, // ignored
          code = 9440771, // OrderAssetPairReversed
          message = s"The $invalidAssetPair asset pair should be reversed"
        )
      )

      wsc.close()
    }

    "correctly send changed tick-size" in {
      placeAndAwaitAtDex(mkOrderDP(alice, ethWavesPair, SELL, 1.eth, 199))

      val wsc     = mkWsOrderBookConnection(ethWavesPair, dex1)
      val buffer0 = wsc.receiveAtLeastN[WsOrderBookChanges](1)

      buffer0 should have size 1
      buffer0.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = ethWavesPair,
          asks = TreeMap(199d -> 1d),
          bids = TreeMap.empty,
          lastTrade = None,
          updateId = 0,
          timestamp = buffer0.last.timestamp,
          settings = WsOrderBookSettings(None, 0.0002.some).some
        )
      )

      wsc.clearMessages()
      placeAndAwaitAtDex(mkOrderDP(alice, ethWavesPair, SELL, 1.eth, 200))

      val buffer1 = wsc.receiveAtLeastN[WsOrderBookChanges](1)

      buffer1.size should (be >= 1 and be <= 2)
      buffer1.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = ethWavesPair,
          asks = TreeMap(200d -> 1d),
          bids = TreeMap.empty,
          lastTrade = None,
          updateId = buffer1.last.updateId,
          timestamp = buffer1.last.timestamp,
          settings = WsOrderBookSettings(None, 0.00000001.some).some
        )
      )

      dex1.api.cancelAll(alice)
      wsc.close()
    }

    "should send a full state after connection" in {
      // Force create an order book to pass a validation in the route
      val firstOrder = mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403)
      placeAndAwaitAtDex(firstOrder)
      dex1.api.cancelAll(carol)
      dex1.api.waitForOrderStatus(firstOrder, ApiOrderStatus.Status.Cancelled)

      markup("No orders")
      val wsc0    = mkWsOrderBookConnection(wavesBtcPair, dex1)
      val buffer0 = wsc0.receiveAtLeastN[WsOrderBookChanges](1)
      wsc0.close()

      buffer0 should have size 1
      buffer0.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap.empty,
          bids = TreeMap.empty,
          lastTrade = None,
          updateId = 0,
          timestamp = buffer0.last.timestamp,
          settings = orderBookSettings
        )
      )

      placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403))

      markup("One order")

      val wsc1    = mkWsOrderBookConnection(wavesBtcPair, dex1)
      val buffer1 = wsc1.receiveAtLeastN[WsOrderBookChanges](1)
      wsc1.close()

      buffer1 should have size 1
      buffer1.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap.empty,
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = None,
          updateId = 0,
          timestamp = buffer1.last.timestamp,
          settings = orderBookSettings
        )
      )

      markup("Two orders")

      placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00012))

      val wsc2    = mkWsOrderBookConnection(wavesBtcPair, dex1)
      val buffer2 = wsc2.receiveAtLeastN[WsOrderBookChanges](1)
      wsc2.close()

      buffer2 should have size 1
      buffer2.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(0.00012d    -> 1d),
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = None,
          updateId = 0,
          timestamp = buffer2.last.timestamp,
          settings = orderBookSettings
        )
      )

      markup("Two orders and trade")

      placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 0.5.waves, 0.00013), ApiOrderStatus.Status.Filled)

      val wsc3    = mkWsOrderBookConnection(wavesBtcPair, dex1)
      val buffer3 = wsc3.receiveAtLeastN[WsOrderBookChanges](1)
      wsc3.close()

      buffer3.size should (be >= 1 and be <= 2)
      buffer3.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(0.00012d    -> 0.5d),
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = WsLastTrade(
            price = 0.00012d,
            amount = 0.5,
            side = OrderType.BUY
          ).some,
          updateId = 0,
          timestamp = buffer3.last.timestamp,
          settings = orderBookSettings
        )
      )

      markup("Four orders")

      List(
        mkOrderDP(carol, wavesBtcPair, SELL, 0.6.waves, 0.00013),
        mkOrderDP(carol, wavesBtcPair, BUY, 0.7.waves, 0.000115)
      ).foreach(placeAndAwaitAtDex(_))

      val wsc4    = mkWsOrderBookConnection(wavesBtcPair, dex1)
      val buffer4 = wsc4.receiveAtLeastN[WsOrderBookChanges](1)
      wsc4.close()

      buffer4.size should (be >= 1 and be <= 2)
      // TODO this test won't check ordering :(
      buffer4.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
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
          timestamp = buffer4.last.timestamp,
          settings = orderBookSettings
        )
      )

      dex1.api.cancelAll(carol)
      Seq(wsc0, wsc1, wsc2, wsc3, wsc4).foreach { _.close() }
    }

    "send updates" in {
      val wsc = mkWsOrderBookConnection(wavesBtcPair, dex1)
      wsc.receiveAtLeastN[WsOrderBookChanges](1)
      wsc.clearMessages()

      markup("A new order")
      placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00012))

      val buffer1 = wsc.receiveAtLeastN[WsOrderBookChanges](1)
      buffer1 should have size 1
      buffer1.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap.empty,
          bids = TreeMap(0.00012d -> 1d),
          lastTrade = None,
          updateId = 1,
          timestamp = buffer1.last.timestamp,
          settings = None
        )
      )
      wsc.clearMessages()

      markup("An execution and adding a new order")
      val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.5.waves, 0.00012)
      placeAndAwaitAtDex(order, ApiOrderStatus.Status.PartiallyFilled)

      val buffer2 = wsc.receiveAtLeastN[WsOrderBookChanges](1)
      buffer2.size should (be >= 1 and be <= 2)
      buffer2.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(0.00012d -> 0.5d),
          bids = TreeMap(0.00012d -> 0d),
          lastTrade = WsLastTrade(
            price = 0.00012d,
            amount = 1,
            side = OrderType.SELL
          ).some,
          updateId = buffer2.last.updateId,
          timestamp = buffer2.last.timestamp,
          settings = None
        )
      )
      wsc.clearMessages()

      dex1.api.cancelAll(carol)
      dex1.api.waitForOrderStatus(order, ApiOrderStatus.Status.Cancelled)

      val buffer3 = wsc.receiveAtLeastN[WsOrderBookChanges](1)
      buffer3.size shouldBe 1
      buffer3.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(0.00012d -> 0d),
          bids = TreeMap.empty,
          lastTrade = None,
          updateId = buffer3.last.updateId,
          timestamp = buffer3.last.timestamp,
          settings = None
        )
      )

      wsc.clearMessages()
      wsc.close()
    }

    "send correct update ids" in {

      def assertUpdateId(connection: WsConnection, expectedUpdateId: Long): Unit = {
        val buffer = connection.receiveAtLeastN[WsOrderBookChanges](1)
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

    "stop send updates after unsubscribe and receive them again after subscribe" in {
      val wsc = mkWsOrderBookConnection(wavesBtcPair, dex1)
      wsc.receiveAtLeastN[WsOrderBookChanges](1)
      wsc.clearMessages()

      markup("Unsubscribe")
      wsc.send(WsUnsubscribe(wavesBtcPair))
      val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00005)
      placeAndAwaitAtDex(order)
      wsc.receiveNoMessages()

      markup("Subscribe")
      wsc.send(WsOrderBookSubscribe(wavesBtcPair, 1))
      wsc.receiveAtLeastN[WsOrderBookChanges](1)
      wsc.clearMessages()

      markup("Update")
      cancelAndAwait(carol, order)
      wsc.receiveAtLeastN[WsOrderBookChanges](1)

      wsc.close()
    }

    "close connections when order book is deleted" in {
      val seller                          = mkAccountWithBalance(100.waves -> Waves)
      val IssueResults(issueTx, _, asset) = mkIssueExtended(seller, "cJIoHoxpeH", 1000.asset8)
      val assetPair                       = AssetPair(asset, Waves)

      broadcastAndAwait(issueTx)
      dex1.api.place(mkOrderDP(seller, assetPair, SELL, 100.asset8, 5.0))

      val wsc1, wsc2, wsc3 = mkWsOrderBookConnection(assetPair, dex1)
      val wscs             = List(wsc1, wsc2, wsc3)
      wscs.foreach { _.receiveAtLeastN[WsOrderBookChanges](1) }

      dex1.api.tryDeleteOrderBook(assetPair)

      val expectedMessage = WsError(
        timestamp = 0L, // ignored
        code = 8388624, // OrderBookStopped
        message = s"The order book for $assetPair is stopped, please contact with the administrator"
      )

      wscs.foreach { wsc =>
        wsc.receiveAtLeastN[WsError](1).head should matchTo(expectedMessage)
        wsc.close()
      }
    }

    "close old subscriptions when order book subscriptions limit has been reached" in {
      Seq(
        mkOrderDP(alice, wavesUsdPair, SELL, 1.waves, 1.0),
        mkOrderDP(alice, wavesBtcPair, SELL, 1.waves, 0.00011119),
        mkOrderDP(alice, ethWavesPair, SELL, 1.eth, 195),
        mkOrderDP(bob, btcUsdPair, SELL, 1.btc, 8698.782732)
      ) foreach { placeAndAwaitAtDex(_) }

      val wsc = mkWsConnection(dex1)

      Seq(wavesUsdPair, wavesBtcPair, ethWavesPair, btcUsdPair, wavesUsdPair).foreach { assetPair =>
        wsc.send(WsOrderBookSubscribe(assetPair, 1))
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
      }

      wsc.receiveAtLeastN[WsError](2) should matchTo {
        List(
          WsError.from(SubscriptionsLimitReached(3, wavesUsdPair.toString), 0L),
          WsError.from(SubscriptionsLimitReached(3, wavesBtcPair.toString), 0L)
        )
      }
    }

    "be opened even if there is no such order book" in {
      val IssueResults(issueTx, _, bch) = mkIssueExtended(alice, "BCHC", 1000.asset8)
      val bchUsdPair                    = AssetPair(bch, usd)

      broadcastAndAwait(issueTx)

      val wsc      = mkWsOrderBookConnection(bchUsdPair, dex1)
      val snapshot = wsc.receiveAtLeastN[WsOrderBookChanges](1).head

      snapshot.asks shouldBe empty
      snapshot.bids shouldBe empty
      wsc.clearMessages()

      placeAndAwaitAtDex { mkOrderDP(alice, bchUsdPair, SELL, 10.asset8, 231.0) }
      wsc.receiveAtLeastN[WsOrderBookChanges](1).head.asks should matchTo(TreeMap(231.0 -> 10.0))
    }
  }
}
