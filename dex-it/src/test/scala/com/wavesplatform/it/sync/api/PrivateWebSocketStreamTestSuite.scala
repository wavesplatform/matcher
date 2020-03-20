package com.wavesplatform.it.sync.api

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsOrder, _}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.api.websockets.HasWebSockets
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder, OrderStatus}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.wavesj.transactions.IssueTransaction

class PrivateWebSocketStreamTestSuite extends MatcherSuiteBase with HasWebSockets {
  private implicit val efc: ErrorFormatterContext = assetDecimalsMap.apply

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    broadcastAndAwait(mkTransfer(alice, bob, 10000.usd, usd, 0.003.waves), mkTransfer(bob, alice, 10.btc, btc, 0.003.waves))
    dex1.start()
    dex1.api.upsertRate(usd, 1)
  }

  "should send account updates to authenticated user" - {

    "when account is empty" in {
      val wsac = mkWebSocketAuthenticatedConnection(mkKeyPair("Test"), dex1)
      wsac.getAllBalances should have size 0
      wsac.getAllOrders should have size 0
      wsac.close()
    }

    "when user place and cancel limit order" in {
      val acc = mkAccountWithBalance(150.usd -> usd)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)
      val bo1 = mkOrder(acc, wavesUsdPair, BUY, 100.waves, 100)
      val bo2 = mkOrder(acc, wavesUsdPair, BUY, 10.waves, 100, version = 3, feeAsset = usd, matcherFee = 30)

      placeAndAwaitAtDex(bo1)
      placeAndAwaitAtDex(bo2)

      eventually { wsc.getAllOrders.size should be >= 2 }
      wsc.getAllBalances.distinct should matchTo(
        Seq(
          usd   -> WsBalances(tradable = 150.0, reserved = 0.0),
          Waves -> WsBalances(tradable = 0.0, reserved = 0.0),
          usd   -> WsBalances(tradable = 50.0, reserved = 100.0),
          usd   -> WsBalances(tradable = 39.7, reserved = 110.3)
        )
      )
      wsc.getAllOrders.distinct should matchTo(
        Seq(
          WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Accepted),
          WsOrder.fromDomain(LimitOrder(bo2), OrderStatus.Accepted)
        )
      )

      wsc.clearMessagesBuffer()

      cancelAndAwait(acc, bo1)

      eventually { wsc.getAllBalances.size should be >= 1 }
      wsc.getAllBalances should contain(usd -> WsBalances(tradable = 139.7, reserved = 10.3))
      wsc.getAllOrders should contain(WsOrder(bo1.id(), status = OrderStatus.Cancelled.name.some))

      cancelAndAwait(acc, bo2)

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllOrders should contain(WsOrder(bo2.id(), status = OrderStatus.Cancelled.name.some))
      wsc.getAllBalances should contain(usd -> WsBalances(tradable = 150.0, reserved = 0.0))
    }

    "when user place and fill market order" in {
      val acc = mkAccountWithBalance(51.003.waves -> Waves)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)
      val smo = mkOrder(acc, wavesUsdPair, SELL, 50.waves, 100)

      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 15.waves, 120))
      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 25.waves, 110))
      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 40.waves, 100))

      dex1.api.placeMarket(smo)
      waitForOrderAtNode(smo)

      eventually { wsc.getAllBalances.size should be >= 3 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 51.003, reserved = 0.0))
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 1.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 55.5, reserved = 0.0))

      wsc.getAllOrders.distinct should matchTo(
        Seq(
          WsOrder
            .fromDomain(MarketOrder(smo, 0.waves), status = OrderStatus.Filled(0.waves, 0.waves))
            .copy(
              filledAmount = 50.0.some,
              filledFee = 0.003.some,
              avgWeighedPrice = 1.11.some // (15*120 + 25*110 + 10*100)/50
            )
        )
      )
    }

    "when user order fully filled with another one" in {
      val acc = mkAccountWithBalance(10.usd -> usd)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)
      eventually { wsc.getAllBalances.size should be >= 1 }

      val bo1 = mkOrder(acc, wavesUsdPair, BUY, 10.waves, 100)

      placeAndAwaitAtDex(bo1)
      dex1.api.place(mkOrder(alice, wavesUsdPair, SELL, 10.waves, 100))

      eventually { wsc.getAllBalances.size should be >= 5 }
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 0.0, reserved = 0.0))
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 9.997, reserved = 0.0))

      wsc.getAllOrders.distinct should matchTo(
        Seq(
          WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Filled(10.waves, 0.003.waves)),
        )
      )
    }

    "when user's order partially filled with another one" in {
      val acc = mkAccountWithBalance(10.usd -> usd)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)
      val bo1 = mkOrder(acc, wavesUsdPair, BUY, 10.waves, 100)

      placeAndAwaitAtDex(bo1)
      placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, SELL, 5.waves, 100))

      eventually { wsc.getAllBalances.size should be >= 5 }
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 0.0, reserved = 5.0))
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 4.9985, reserved = 0.0))

      wsc.getAllOrders.distinct should matchTo(
        Seq(
          WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Accepted),
          WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Filled(5.waves, 0.0015.waves)),
        )
      )
    }

    "when user make a transfer" in {
      val acc = mkAccountWithBalance(10.waves -> Waves, 10.usd -> usd)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 10.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 10.0, reserved = 0.0))

      wsc.clearMessagesBuffer()

      broadcastAndAwait(mkTransfer(acc, alice.toAddress, 2.usd, usd, feeAmount = 1.waves))

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 9.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 8.0, reserved = 0.0))
    }

    "user had issued a new asset after the connection already established" ignore { //TODO: bug
      val acc = mkAccountWithBalance(10.waves -> Waves)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)

      val txIssue: IssueTransaction = mkIssue(acc, "testAsset", 1000.waves, 8)
      broadcastAndAwait(txIssue)

      wsc.getAllBalances should contain(Waves         -> WsBalances(tradable = 9.0, reserved = 0.0))
      wsc.getAllBalances should contain(txIssue.getId -> WsBalances(tradable = 1000.0, reserved = 0.0))
    }

    "user had issued a new asset before establishing the connection" ignore { //TODO: bug
      val acc = mkAccountWithBalance(10.waves -> Waves)

      val txIssue: IssueTransaction = mkIssue(acc, "testAsset", 1000.waves, 8)
      broadcastAndAwait(txIssue)

      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)

      eventually { wsc.getAllBalances.size should be >= 1 }
      wsc.getAllBalances should contain(Waves         -> WsBalances(tradable = 9.0, reserved = 0.0))
      wsc.getAllBalances should contain(txIssue.getId -> WsBalances(tradable = 1000.0, reserved = 0.0))
    }

    "user burnt part of the asset amount" in {
      val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 10.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 20.0, reserved = 0.0))

      broadcastAndAwait(mkBurn(acc, usd, 10.usd))

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 9.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 10.0, reserved = 0.0))
    }

    "user burnt all of the asset amount" in {
      val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
      val wsc = mkWebSocketAuthenticatedConnection(acc, dex1)

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 10.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 20.0, reserved = 0.0))

      wsc.clearMessagesBuffer()

      broadcastAndAwait(mkBurn(acc, usd, 20.usd))

      eventually { wsc.getAllBalances.size should be >= 2 }
      wsc.getAllBalances should contain(Waves -> WsBalances(tradable = 9.0, reserved = 0.0))
      wsc.getAllBalances should contain(usd   -> WsBalances(tradable = 0.0, reserved = 0.0))
    }
  }
}
