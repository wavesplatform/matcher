package com.wavesplatform.it.sync.api

import akka.http.scaladsl.model.ws.Message
import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsAddressState, WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.responses.dex.{OrderStatus => ResponseOrderStatus}
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WebSocketAuthenticatedConnection}
import com.wavesplatform.dex.model.{LimitOrder, OrderStatus}
import com.wavesplatform.it.MatcherSuiteBase

class MatcherWebSocketsTestSuite extends MatcherSuiteBase with HasWebSockets {

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

  "Connection should be established" in {

    val wsUri                           = s"127.0.0.1:${dex1.restApiAddress.getPort}/ws/time"
    val outputParser: Message => String = _.asTextMessage.getStrictText

    val wscMobile = mkWebSocketConnection(wsUri, outputParser)
    val wscWeb    = mkWebSocketConnection(wsUri, outputParser, trackOutput = false)
    val wscTest   = mkWebSocketConnection(wsUri, outputParser)

    Thread.sleep(2000)

    val wscDesktop = mkWebSocketConnection(wsUri, outputParser)

    Thread.sleep(3000)

    Seq(wscMobile, wscDesktop, wscTest).foreach { connection =>
      connection.close()
      connection.getMessagesBuffer.foreach(_ should startWith("Now is"))
    }

    wscTest.clearMessagesBuffer()

    wscMobile.getMessagesBuffer.size should be > wscDesktop.getMessagesBuffer.size
    Seq(wscWeb, wscTest).foreach {
      _.getMessagesBuffer.size shouldBe 0
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
            balances = Map(Waves -> WsBalances(100.waves, 0), btc -> WsBalances(1.btc, 0)),
            orders = Seq.empty
          )
        )

        val buyOrder  = mkOrderDP(carol, wavesBtcPair, BUY, 1.waves, 0.00011403)
        val sellOrder = mkOrderDP(carol, wavesUsdPair, SELL, 1.waves, 3.01)

        placeAndAwaitAtDex(buyOrder)
        placeAndAwaitAtDex(sellOrder)

        Seq(buyOrder, sellOrder).foreach { o =>
          dex1.api.cancel(carol, o)
          dex1.api.waitForOrderStatus(o, ResponseOrderStatus.Cancelled)
        }

        assertAddressStateChanges(
          connection = wsac,
          balancesChangesCountBorders = (2, 4),
          ordersChangesCount = 4,
          expectedBalanceChanges = squashBalanceChanges(
            Seq(
              Map(btc   -> WsBalances(tradable = 0.99988597.btc, reserved = 0.00011403.btc)), // reserve
              Map(Waves -> WsBalances(tradable = 98.997.waves, reserved = 1.003.waves)), // reserve
              Map(btc   -> WsBalances(tradable = 1.btc, reserved = 0)), // cancel
              Map(Waves -> WsBalances(tradable = 100.waves, reserved = 0)) // cancel
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
            balances = Map(Waves -> WsBalances(100.waves, 0), btc -> WsBalances(1.btc, 0), usd -> WsBalances(200.usd, 0)),
            orders = Seq.empty
          )
        )

        placeAndAwaitAtDex(mkOrderDP(bob, wavesUsdPair, SELL, 6.waves, 3.0))

        val buyOrder = mkOrderDP(carol, wavesUsdPair, BUY, 12.waves, 3.0, 0.00000034.btc, btc)

        placeAndAwaitAtNode(buyOrder)

        dex1.api.cancel(carol, buyOrder)
        dex1.api.waitForOrderStatus(buyOrder, ResponseOrderStatus.Cancelled)

        assertAddressStateChanges(
          connection = wsac,
          balancesChangesCountBorders = (3, 8),
          ordersChangesCount = 2,
          expectedBalanceChanges = squashBalanceChanges(
            Seq(
              Map(usd   -> WsBalances(200.usd, 0)), // transfer
              Map(usd   -> WsBalances(164.usd, 36.usd), btc -> WsBalances(0.99999966.btc, 0.00000034.btc)), // reserve
              Map(usd   -> WsBalances(164.usd, 18.usd), btc -> WsBalances(0.99999966.btc, 0.00000017.btc)), // execution
              Map(Waves -> WsBalances(106.waves, 0)), // execution
              Map(usd   -> WsBalances(182.usd, 0), btc -> WsBalances(0.99999983.btc, 0)) // cancelling
            )
          ),
          expectedOrdersChanges = Seq(
            WsOrder
              .fromDomain(LimitOrder(buyOrder), OrderStatus.PartiallyFilled(6.waves, 0.00000017.btc))
              .copy(
                filledAmount = 6.waves.some,
                filledFee = 0.00000017.btc.some,
                avgFilledPrice = 3.0.usd.some
              ),
            WsOrder(buyOrder.id(), status = OrderStatus.Cancelled.name.some)
          )
        )

        wsac.close()
      }
    }
  }
}
