package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsOrder, _}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.it.api.websockets.{HasWebSockets, WsAuthenticatedConnection}
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder, OrderStatus}
import com.wavesplatform.it.MatcherSuiteBase

class WebSocketPrivateStreamTestSuite extends MatcherSuiteBase with HasWebSockets {

  private implicit val efc: ErrorFormatterContext = assetDecimalsMap.apply

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
    dex1.api.upsertRate(usd, 1)
  }

  override def afterEach(): Unit = dex1.api.cancelAll(alice)

  private object AuthMethod extends Enumeration { type AuthMethod = Value; val Signature, ApiKey = Value }; import AuthMethod._

  private def mkWsConnection(account: KeyPair, method: AuthMethod): WsAuthenticatedConnection = method match {
    case Signature => mkWsAuthenticatedConnection(account, dex1)
    case ApiKey    => mkWsAuthenticatedConnectionViaApiKey(account, dex1)
  }

  private def assertChanges(c: WsAuthenticatedConnection, squash: Boolean = true)(expBs: Map[Asset, WsBalances]*)(expOs: WsOrder*): Unit = {

    def squashBalances(bs: Seq[Map[Asset, WsBalances]]): Map[Asset, WsBalances] = bs.foldLeft(Map.empty[Asset, WsBalances])(_ ++ _)

    def squashOrders(os: Seq[WsOrder]): Seq[WsOrder] = {
      os.groupBy(_.id)
        .mapValues { orderChanges =>
          orderChanges
            .foldLeft(orderChanges.head) {
              case (acc, oc) =>
                acc.copy(status = oc.status, filledAmount = oc.filledAmount, filledFee = oc.filledFee, avgWeighedPrice = oc.avgWeighedPrice)
            }
        }
        .values
        .toSeq
    }

    eventually {
      if (squash) {
        c.getBalancesChanges.size should be <= expBs.size
        squashBalances(c.getBalancesChanges) should matchTo { squashBalances(expBs) }
        c.getOrderChanges.size should be <= expOs.size
        squashOrders(c.getOrderChanges) should matchTo { squashOrders(expOs) }
      } else {
        c.getBalancesChanges should matchTo(expBs)
        c.getOrderChanges should matchTo(expOs)
      }
    }

    c.clearMessagesBuffer()
  }

  Seq(Signature, ApiKey).foreach { method =>
    s"Private stream should send account updates to authenticated (via $method) user" - {

      "when account is empty" in {
        val wsac = mkWsConnection(mkKeyPair("Test"), method)
        eventually { wsac.getMessagesBuffer should have size 1 }
        assertChanges(wsac, squash = false)()()
        wsac.close()
      }

      "when user places and cancels limit order" in {

        val acc = mkAccountWithBalance(150.usd -> usd, 10.waves -> Waves)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(150.0, 0.0)) }()

        val bo1 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0)
        val bo2 = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0, feeAsset = usd, matcherFee = 0.3.usd)

        Seq(bo1, bo2).foreach { placeAndAwaitAtDex(_) }

        assertChanges(wsc)(
          Map(usd -> WsBalances(50, 100)), // TODO after 2.0.x. merge: Map(usd -> WsBalances(50, 100), Waves -> WsBalances(9.997, 0.003))
          Map(usd -> WsBalances(39.70, 110.30))
        )(
          WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Accepted),
          WsOrder.fromDomain(LimitOrder(bo2), OrderStatus.Accepted)
        )

        cancelAndAwait(acc, bo1)
        assertChanges(wsc, squash = false) { Map(usd -> WsBalances(139.70, 10.30)) } { WsOrder(bo1.id(), OrderStatus.Cancelled.name) }

        cancelAndAwait(acc, bo2)
        assertChanges(wsc, squash = false) { Map(usd -> WsBalances(150, 0)) } { WsOrder(bo2.id(), OrderStatus.Cancelled.name) }

        wsc.close()
      }

      "when user places market orders and it is filled" in {

        val tradableBalance: Map[Asset, Long] = Map(Waves -> 51.003.waves)
        val acc                               = mkAccountWithBalance(tradableBalance(Waves) -> Waves)
        val wsc                               = mkWsConnection(acc, method)
        val smo                               = mkOrderDP(acc, wavesUsdPair, SELL, 50.waves, 1.0)
        val mo                                = MarketOrder(smo, tradableBalance.apply _)

        assertChanges(wsc, squash = false)(Map(Waves -> WsBalances(51.003, 0)))()

        Seq(
          15.waves -> 1.2,
          25.waves -> 1.1,
          40.waves -> 1.0
        ).foreach { case (a, p) => placeAndAwaitAtDex(mkOrderDP(alice, wavesUsdPair, BUY, a, p)) }

        dex1.api.placeMarket(smo)
        waitForOrderAtNode(smo)

        import OrderStatus._

        assertChanges(wsc)(
          Map(Waves -> WsBalances(1, 50.003)),
          Map(Waves -> WsBalances(1, 35.0021), usd -> WsBalances(18, 0)),
          Map(Waves -> WsBalances(1, 10.0006), usd -> WsBalances(45.5, 0)),
          Map(Waves -> WsBalances(1, 0), usd -> WsBalances(55.5, 0)),
        )(
          WsOrder.fromDomain(mo, status = Accepted),
          WsOrder(mo.id, status = PartiallyFilled.name, filledAmount = 15.0, filledFee = 0.0009, avgWeighedPrice = 1.2),
          WsOrder(mo.id, status = PartiallyFilled.name, filledAmount = 40.0, filledFee = 0.0024, avgWeighedPrice = 1.1375),
          WsOrder(mo.id, status = Filled.name, filledAmount = 50.0, filledFee = 0.003, avgWeighedPrice = 1.11)
        )

        wsc.close()
      }

      "when user's order is fully filled with another one" in {

        val acc = mkAccountWithBalance(10.usd -> usd, 10.waves -> Waves)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()

        val bo = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0)

        placeAndAwaitAtDex(bo)
        placeAndAwaitAtNode(mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 1.0))

        assertChanges(wsc)(
          Map(usd   -> WsBalances(0, 10)), // TODO: after 2.0.x merge: Map(usd -> WsBalances(0, 10), Waves -> (9.997, 0.003))
          Map(usd   -> WsBalances(0, 0)),
          Map(Waves -> WsBalances(19.997, 0)) // since balance increasing comes after transaction mining
        )(
          WsOrder.fromDomain(LimitOrder(bo), OrderStatus.Accepted),
          WsOrder(bo.id(), status = OrderStatus.Filled.name, filledAmount = 10.0, filledFee = 0.003, avgWeighedPrice = 1.0)
        )

        wsc.close()
      }

      "when user's order is partially filled with another one" in {

        val acc = mkAccountWithBalance(10.usd -> usd, 10.waves -> Waves)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()

        val bo         = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0)
        val limitOrder = LimitOrder(bo)

        placeAndAwaitAtDex(bo)
        placeAndAwaitAtNode(mkOrderDP(alice, wavesUsdPair, SELL, 5.waves, 1.0))

        assertChanges(wsc)(
          Map(usd   -> WsBalances(0, 10)), // TODO: after 2.0.x merge: Map(usd -> WsBalances(0, 10), Waves -> (9.997, 0.003))
          Map(usd   -> WsBalances(0, 5)),
          Map(Waves -> WsBalances(14.9985, 0)) // since balance increasing comes after transaction mining
        )(
          WsOrder.fromDomain(limitOrder, OrderStatus.Accepted),
          WsOrder(limitOrder.id, status = OrderStatus.PartiallyFilled.name, filledAmount = 5.0, filledFee = 0.0015, avgWeighedPrice = 1.0)
        )

        dex1.api.cancelAll(acc)
        assertChanges(wsc, squash = false) { Map(usd -> WsBalances(5, 0)) } { WsOrder(bo.id(), status = OrderStatus.Cancelled.name) }

        wsc.close()
      }

      "when user make a transfer" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 10.usd -> usd)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()
        broadcastAndAwait(mkTransfer(acc, alice.toAddress, 2.usd, usd, feeAmount = 1.waves))
        assertChanges(wsc) { Map(Waves -> WsBalances(9, 0), usd -> WsBalances(8, 0)) }()

        wsc.close()
      }

      "user issued a new asset after establishing the connection" in {

        val acc = mkAccountWithBalance(10.waves -> Waves)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0)) }()
        val IssueResults(txIssue, _, issuedAsset) = mkIssueExtended(acc, "testAsset", 1000.asset8)
        broadcastAndAwait(txIssue)

        assertChanges(wsc)(
          Map(Waves       -> WsBalances(9, 0)),
          Map(issuedAsset -> WsBalances(1000, 0))
        )()

        wsc.close()
      }

      "user issued a new asset before establishing the connection" in {

        val acc                                   = mkAccountWithBalance(10.waves -> Waves)
        val IssueResults(txIssue, _, issuedAsset) = mkIssueExtended(acc, "testAsset", 1000.asset8)

        broadcastAndAwait(txIssue)

        val wsc = mkWsConnection(acc, method)
        assertChanges(wsc) { Map(Waves -> WsBalances(9, 0), issuedAsset -> WsBalances(1000, 0)) }()

        wsc.close()
      }

      "user burnt part of the asset amount" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(20, 0)) }()
        broadcastAndAwait(mkBurn(acc, usd, 10.usd))
        assertChanges(wsc) { Map(Waves -> WsBalances(9, 0), usd -> WsBalances(10, 0)) }()

        wsc.close()
      }

      "user burnt all of the asset amount" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
        val wsc = mkWsConnection(acc, method)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(20, 0)) }()
        broadcastAndAwait(mkBurn(acc, usd, 20.usd))
        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(9, 0), usd -> WsBalances(0, 0)) }()

        wsc.close()
      }
    }
  }

  "Second connection should get the actual data" in {

    val acc  = mkAccountWithBalance(500.usd -> usd, 10.waves -> Waves)
    val wsc1 = mkWsAuthenticatedConnection(acc, dex1)

    assertChanges(wsc1, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(500, 0)) }()

    val now = System.currentTimeMillis()

    val bo1 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0, ts = now)
    val bo2 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0, ts = now + 1)

    Seq(bo1, bo2).foreach { placeAndAwaitAtDex(_) }

    assertChanges(wsc1)(
      Map(usd -> WsBalances(400, 100)),
      Map(usd -> WsBalances(300, 200))
    )(
      WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Accepted),
      WsOrder.fromDomain(LimitOrder(bo2), OrderStatus.Accepted)
    )

    val wsc2 = mkWsAuthenticatedConnection(acc, dex1)

    assertChanges(wsc2) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(300, 200)) }(
      WsOrder.fromDomain(LimitOrder(bo1), OrderStatus.Accepted),
      WsOrder.fromDomain(LimitOrder(bo2), OrderStatus.Accepted)
    )

    Seq(wsc1, wsc2).foreach { _.close() }
  }
}
