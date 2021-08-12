package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.{WsLastTrade, WsOrderBookSettings}
import com.wavesplatform.dex.api.ws.protocol
import com.wavesplatform.dex.api.ws.protocol.{WsError, WsOrderBookChanges, WsOrderBookSubscribe, WsUnsubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{OrderAssetPairReversed, OrderBookStopped, SubscriptionsLimitReached}
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.Implicits.releasable
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, OrderRestrictionsSettings}
import com.wavesplatform.it.api.MatcherCommand
import com.wavesplatform.it.{executeCommands, WsSuiteBase}
import play.api.libs.json._

import scala.collection.immutable.TreeMap
import scala.concurrent.Future
import scala.util.{Random, Using}

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
       |  web-sockets.external-client-handler.subscriptions.max-order-book-number = 3
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx, IssueEthTx)
    broadcastAndAwait(mkTransfer(alice, carol, 100.waves, Waves), mkTransfer(bob, carol, 1.btc, btc))
    dex1.start()
    dex1.api.upsertAssetRate(btc, 0.00011167)
  }

  "Order book stream should" - {

    "correctly handle rejections" in {
      val invalidAssetPair = AssetPair(Waves, eth)

      Using.resource(mkDexWsConnection(dex1)) { wsc =>
        wsc.send(WsOrderBookSubscribe(invalidAssetPair, 1))

        wsc.receiveAtLeastN[WsError](1).head should matchTo(
          WsError(
            timestamp = 0L, // ignored
            code = OrderAssetPairReversed.code,
            message = s"The $invalidAssetPair asset pair should be reversed"
          )
        )
      }
    }

    "correctly send changed tick-size" in {
      placeAndAwaitAtDex(mkOrderDP(alice, ethWavesPair, SELL, 1.eth, 199))

      Using.resource(mkWsOrderBookConnection(ethWavesPair, dex1)) { wsc =>
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

        // An aggregated order book could not be updated
        eventually {
          val buffer = wsc.receiveAtLeastN[WsOrderBookChanges](1)

          buffer.size should (be >= 1 and be <= 2)
          buffer.squashed.values.head should matchTo(
            protocol.WsOrderBookChanges(
              assetPair = ethWavesPair,
              asks = TreeMap(200d -> 1d),
              bids = TreeMap.empty,
              lastTrade = None,
              updateId = buffer.last.updateId,
              timestamp = buffer.last.timestamp,
              settings = WsOrderBookSettings(None, 0.00000001.some).some
            )
          )
        }

        dex1.api.cancelAllOrdersWithSig(alice)
      }
    }

    "should send a full state after connection" in {
      // Force create an order book to pass a validation in the route
      val firstOrder = mkOrderDP(carol, wavesBtcPair, BUY, 1.05.waves, 0.00011403)
      placeAndAwaitAtDex(firstOrder)
      dex1.api.cancelAllOrdersWithSig(carol)
      dex1.api.waitForOrderStatus(firstOrder, HttpOrderStatus.Status.Cancelled)

      markup("No orders")
      val buffer0 = Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
      }
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

      val buffer1 = Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
      }
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

      val buffer2 = Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
      }

      buffer2 should have size 1
      buffer2.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(0.00012d -> 1d),
          bids = TreeMap(0.00011403d -> 1.05d),
          lastTrade = None,
          updateId = 0,
          timestamp = buffer2.last.timestamp,
          settings = orderBookSettings
        )
      )

      markup("Two orders and trade")

      placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 0.5.waves, 0.00013), HttpOrderStatus.Status.Filled)

      val buffer3 = Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
      }

      buffer3.size should (be >= 1 and be <= 2)
      buffer3.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(0.00012d -> 0.5d),
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

      val buffer4 = Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
      }

      buffer4.size should (be >= 1 and be <= 2)
      buffer4.squashed.values.head should matchTo(
        protocol.WsOrderBookChanges(
          assetPair = wavesBtcPair,
          asks = TreeMap(
            0.00012d -> 0.5d,
            0.00013d -> 0.6d
          ),
          bids = TreeMap(
            0.000115d -> 0.7d,
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

      dex1.api.cancelAllOrdersWithSig(carol)
    }

    "send updates" in {
      Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
        wsc.receiveAtLeastN[WsOrderBookChanges](1)
        wsc.clearMessages()

        markup("A new order")
        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00012))

        eventually {
          val buffer = wsc.receiveAtLeastN[WsOrderBookChanges](1)
          buffer should have size 1
          buffer.squashed.values.head should matchTo(
            protocol.WsOrderBookChanges(
              assetPair = wavesBtcPair,
              asks = TreeMap.empty,
              bids = TreeMap(0.00012d -> 1d),
              lastTrade = None,
              updateId = 1,
              timestamp = buffer.last.timestamp,
              settings = None
            )
          )
        }
        wsc.clearMessages()

        markup("An execution and adding a new order")
        val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.5.waves, 0.00012)
        placeAndAwaitAtDex(order, HttpOrderStatus.Status.PartiallyFilled)

        eventually {
          val buffer = wsc.receiveAtLeastN[WsOrderBookChanges](1)
          buffer.size should (be >= 1 and be <= 2)
          buffer.squashed.values.head should matchTo(
            protocol.WsOrderBookChanges(
              assetPair = wavesBtcPair,
              asks = TreeMap(0.00012d -> 0.5d),
              bids = TreeMap(0.00012d -> 0d),
              lastTrade = WsLastTrade(
                price = 0.00012d,
                amount = 1,
                side = OrderType.SELL
              ).some,
              updateId = buffer.last.updateId,
              timestamp = buffer.last.timestamp,
              settings = None
            )
          )
        }
        wsc.clearMessages()

        dex1.api.cancelAllOrdersWithSig(carol)
        dex1.api.waitForOrderStatus(order, HttpOrderStatus.Status.Cancelled)

        eventually {
          val buffer = wsc.receiveAtLeastN[WsOrderBookChanges](1)
          buffer.size shouldBe 1
          buffer.squashed.values.head should matchTo(
            protocol.WsOrderBookChanges(
              assetPair = wavesBtcPair,
              asks = TreeMap(0.00012d -> 0d),
              bids = TreeMap.empty,
              lastTrade = None,
              updateId = buffer.last.updateId,
              timestamp = buffer.last.timestamp,
              settings = None
            )
          )
        }
        wsc.clearMessages()

      }
    }

    "send correct update ids" in {

      def assertUpdateId(connection: WsConnection, expectedUpdateId: Long): Unit = {
        val buffer = connection.receiveAtLeastN[WsOrderBookChanges](1)
        buffer should have size 1
        buffer.head.updateId shouldBe expectedUpdateId
        connection.clearMessages()
      }

      val order = mkOrderDP(carol, wavesBtcPair, SELL, 1.waves, 0.00005)

      Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc1 =>
        assertUpdateId(wsc1, 0)

        placeAndAwaitAtDex(order)
        assertUpdateId(wsc1, 1)

        Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc2 =>
          assertUpdateId(wsc2, 0)

          dex1.api.cancelOneOrAllInPairOrdersWithSig(carol, order)
          assertUpdateId(wsc1, 2)
          assertUpdateId(wsc2, 1)
        }
      }
    }

    "stop send updates after unsubscribe and receive them again after subscribe" in {
      Using.resource(mkWsOrderBookConnection(wavesBtcPair, dex1)) { wsc =>
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

      }
    }

    "close connections when order book is deleted" in {
      val seller = mkAccountWithBalance(100.waves -> Waves)
      val IssueResults(issueTx, _, asset) = mkIssueExtended(seller, "cJIoHoxpeH", 1000.asset8)
      val assetPair = AssetPair(asset, Waves)

      broadcastAndAwait(issueTx)
      dex1.api.place(mkOrderDP(seller, assetPair, SELL, 100.asset8, 5.0))

      Using.resource((1 to 3).map(_ => mkWsOrderBookConnection(assetPair, dex1))) { wscs =>
        wscs.foreach(_.receiveAtLeastN[WsOrderBookChanges](1))

        dex1.tryApi.deleteOrderBookWithKey(assetPair)

        val expectedMessage = WsError(
          timestamp = 0L, // ignored
          code = OrderBookStopped.code,
          message = s"The order book for $assetPair is stopped, please contact with the administrator"
        )

        wscs.foreach { wsc =>
          wsc.receiveAtLeastN[WsError](1).head should matchTo(expectedMessage)
        }
      }
    }

    "close old subscriptions when order book subscriptions limit has been reached" in {
      Seq(
        mkOrderDP(alice, wavesUsdPair, SELL, 1.waves, 1.0),
        mkOrderDP(alice, wavesBtcPair, SELL, 1.waves, 0.00011119),
        mkOrderDP(alice, ethWavesPair, SELL, 1.eth, 195),
        mkOrderDP(bob, btcUsdPair, SELL, 1.btc, 8698.782732)
      ).foreach(placeAndAwaitAtDex(_))

      val wsc = mkDexWsConnection(dex1)

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

      dex1.api.cancelAllOrdersWithSig(alice)
    }

    "be opened even if there is no such order book" in {
      val IssueResults(issueTx, _, bch) = mkIssueExtended(alice, "BCHC", 1000.asset8)
      val bchUsdPair = AssetPair(bch, usd)

      broadcastAndAwait(issueTx)

      Using.resource(mkWsOrderBookConnection(bchUsdPair, dex1)) { wsc =>
        val snapshot = wsc.receiveAtLeastN[WsOrderBookChanges](1).head

        snapshot.asks shouldBe empty
        snapshot.bids shouldBe empty
        wsc.clearMessages()

        placeAndAwaitAtDex(mkOrderDP(alice, bchUsdPair, SELL, 10.asset8, 231.0))
        wsc.receiveAtLeastN[WsOrderBookChanges](1).head.asks should matchTo(TreeMap(231.0 -> 10.0))
      }
    }

    def placeOrdersAsync(t: OrderType): Unit = {
      val orders = (1 until 50).map(_ => mkOrderDP(alice, wavesUsdPair, t, 1.usd, Random.between(10000, 10000000)))
      executeCommands(Random.shuffle(orders.map(MatcherCommand.Place(dex1, _))))
      orders.foreach(o => eventually(dex1.api.waitForOrderStatus(o, Status.Accepted)))
    }

    "send prices in bids with right order" in {
      Using.resource(mkDexWsConnection(dex1)) { wsc =>
        wsc.send(WsOrderBookSubscribe(wavesUsdPair, 1))

        placeOrdersAsync(BUY)

        wsc.receiveAtLeastNRaw(1).map(m =>
          (Json.parse(m.body) \ "b").asOpt[List[List[String]]] match {
            case Some(b) => b.flatMap(_.head).map(_.toDouble)
            case None => List.empty
          }
        ).filter(l => l == l.sorted && l.size > 1) should have size 0

        dex1.api.cancelAllOrdersWithSig(alice)
      }
    }

    "send prices in asks with right order" in {
      Using.resource(mkDexWsConnection(dex1)) { wsc =>
        wsc.send(WsOrderBookSubscribe(wavesUsdPair, 1))

        placeOrdersAsync(SELL)

        wsc.receiveAtLeastNRaw(1).map(m =>
          (Json.parse(m.body) \ "a").asOpt[List[List[String]]] match {
            case Some(b) => b.flatMap(_.head).map(_.toDouble)
            case None => List.empty
          }
        ).filter(l => l == l.sorted(Ordering[Double].reverse) && l.size > 1) should have size 0

        dex1.api.cancelAllOrdersWithSig(alice)
      }
    }
  }

  "Bugs" - {
    "DEX-814 Connections can affect each other" in {
      Using.Manager { use =>
        val wscs = use((1 to 10).map(_ => mkWsOrderBookConnection(wavesBtcPair, dex1)))
        val mainWsc = use(mkWsOrderBookConnection(wavesBtcPair, dex1))

        markup("Multiple orders")
        val orders = (1 to 50).map { i =>
          mkOrderDP(carol, wavesBtcPair, BUY, 1.waves + i, 0.00012 + i / 100000.0d)
        }

        Future.traverse(orders)(dex1.asyncApi.place).futureValue
        dex1.api.cancelAllOrdersWithSig(carol)

        Future.traverse(wscs)(wsc => Future(wsc.close())).futureValue
        Thread.sleep(3000)
        mainWsc.clearMessages()

        markup("A new order")
        placeAndAwaitAtDex(mkOrderDP(carol, wavesBtcPair, BUY, 2.waves, 0.00029))

        eventually {
          val buffer = mainWsc.receiveAtLeastN[WsOrderBookChanges](1)
          buffer.squashed.values.head.copy(updateId = 0) should matchTo(
            WsOrderBookChanges(
              assetPair = wavesBtcPair,
              asks = TreeMap.empty,
              bids = TreeMap(0.00029d -> 2d),
              lastTrade = none,
              updateId = 0,
              timestamp = buffer.last.timestamp,
              settings = none
            )
          )
        }
        mainWsc.clearMessages()
      }
    }
  }
}
