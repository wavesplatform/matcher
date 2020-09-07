package com.wavesplatform.it.sync.api.ws

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.ws.connection.WsConnection
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.api.ws.protocol.{WsAddressChanges, WsAddressSubscribe, WsError, WsUnsubscribe}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.SubscriptionsLimitReached
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.dex.model.{LimitOrder, MarketOrder, OrderStatus}
import com.wavesplatform.it.WsSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.parallel.CollectionConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class WsAddressStreamTestSuite extends WsSuiteBase with TableDrivenPropertyChecks {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |  web-sockets.external-client-handler.subscriptions.max-address-number = 3
         |}""".stripMargin)
    .withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
    dex1.api.upsertRate(usd, 1)
  }

  override def afterEach(): Unit = dex1.api.cancelAll(alice)

  private def mkWsAddressConnection(account: KeyPair): WsConnection = mkWsAddressConnection(account, dex1)

  "Address stream should" - {

    "correctly handle rejections" in {
      val fooAddress = mkKeyPair("foo").toAddress
      val barKeyPair = mkKeyPair("bar")

      val wsc = mkDexWsConnection(dex1)
      wsc.send(
        WsAddressSubscribe(
          fooAddress,
          WsAddressSubscribe.defaultAuthType,
          mkJwt(barKeyPair)
        )
      )

      val errors = wsc.receiveAtLeastN[WsError](1)
      errors.head should matchTo(
        WsError(
          timestamp = 0L, // ignored
          code = 106957828, // AddressAndPublicKeyAreIncompatible
          message = "Address 3Q6LEwEVJVAomd4BjjjSPydZuNN4vDo3fSs and public key 54gGdY9o2vFgzkSMLXQ7iReTJMPo2XiGdaBQSsG5U3un are incompatible"
        )
      )

      wsc.close()
    }

    "stop send updates after closing by user and resend after user open it again" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)
      val wsc = mkWsAddressConnection(acc, dex1)

      eventually { wsc.balanceChanges should have size 1 }
      wsc.close()

      broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
      wsc.balanceChanges should have size 1

      val wsc2 = mkWsAddressConnection(acc, dex1)
      eventually { wsc2.balanceChanges should have size 1 }
    }

    "stop send updates after unsubscribe and receive them again after subscribe" in {
      val acc = mkAccountWithBalance(10.waves -> Waves)

      val wsc = mkWsAddressConnection(acc, dex1)
      wsc.receiveAtLeastN[WsAddressChanges](1)
      wsc.clearMessages()

      markup("Unsubscribe")
      wsc.send(WsUnsubscribe(acc))
      broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
      wsc.receiveNoMessages()

      markup("Subscribe")
      wsc.send(WsAddressSubscribe(acc, WsAddressSubscribe.defaultAuthType, mkJwt(acc)))
      wsc.receiveAtLeastN[WsAddressChanges](1)
      wsc.clearMessages()

      markup("Update")
      broadcastAndAwait(mkTransfer(alice, acc.toAddress, 2.usd, usd, feeAmount = 1.waves))
      wsc.receiveAtLeastN[WsAddressChanges](1)

      wsc.close()
    }

    "send account updates to authenticated user" - {

      "when account is empty" in {
        val account      = mkKeyPair("Test")
        val wsac         = mkWsAddressConnection(account)
        val addressState = wsac.receiveAtLeastN[WsAddressChanges](1).head
        addressState.address shouldBe account.toAddress
        assertChanges(wsac, squash = false)()()
        wsac.close()
      }

      "when user places and cancels limit orders" in {

        val acc = mkAccountWithBalance(150.usd -> usd, 10.waves -> Waves)
        val wsc = mkWsAddressConnection(acc)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(150.0, 0.0)) }()

        val bo1 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0)
        val bo2 = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0, feeAsset = usd, matcherFee = 0.3.usd)

        Seq(bo1, bo2).foreach { placeAndAwaitAtDex(_) }

        assertChanges(wsc)(
          Map(usd -> WsBalances(50, 100), Waves -> WsBalances(9.997, 0.003)),
          Map(usd -> WsBalances(39.70, 110.30))
        )(
          WsOrder.fromDomain(LimitOrder(bo1)),
          WsOrder.fromDomain(LimitOrder(bo2))
        )

        cancelAndAwait(acc, bo1)
        assertChanges(wsc, squash = false) { Map(usd -> WsBalances(139.70, 10.30), Waves -> WsBalances(10, 0)) }(
          WsOrder(bo1.id(), OrderStatus.Cancelled.name)
        )

        cancelAndAwait(acc, bo2)
        assertChanges(wsc, squash = false) { Map(usd -> WsBalances(150, 0)) }(
          WsOrder(bo2.id(), OrderStatus.Cancelled.name)
        )

        wsc.close()
      }

      "when user places market order and it is filled" in {

        val tradableBalance: Map[Asset, Long] = Map(Waves -> 51.003.waves)
        val acc                               = mkAccountWithBalance(tradableBalance(Waves) -> Waves)
        val wsc                               = mkWsAddressConnection(acc)
        val smo                               = mkOrderDP(acc, wavesUsdPair, SELL, 50.waves, 1.0)
        val mo                                = MarketOrder(smo, tradableBalance.apply _)

        Seq(
          15.waves -> 1.2,
          25.waves -> 1.1,
          40.waves -> 1.0
        ).foreach { case (a, p) => placeAndAwaitAtDex(mkOrderDP(alice, wavesUsdPair, BUY, a, p)) }

        dex1.api.placeMarket(smo)
        waitForOrderAtNode(smo)

        assertChanges(wsc)(
          Map(Waves -> WsBalances(1, 50.003)),
          Map(Waves -> WsBalances(1, 35.0021)),
          Map(Waves -> WsBalances(1, 10.0006)),
          Map(Waves -> WsBalances(1, 0)),
          Map(usd   -> WsBalances(18, 0)),
          Map(usd   -> WsBalances(45.5, 0)),
          Map(usd   -> WsBalances(55.5, 0))
        )(
          WsOrder.fromDomain(mo),
          WsOrder(mo.id, status = OrderStatus.PartiallyFilled.name, filledAmount = 15.0, filledFee = 0.0009, avgWeighedPrice = 1.2),
          WsOrder(mo.id, status = OrderStatus.PartiallyFilled.name, filledAmount = 40.0, filledFee = 0.0024, avgWeighedPrice = 1.1375),
          WsOrder(mo.id, status = OrderStatus.Filled.name, filledAmount = 50.0, filledFee = 0.003, avgWeighedPrice = 1.11)
        )

        wsc.close()
        dex1.api.cancelAll(alice)
      }

      "when user's order is fully filled with another one" in {

        val acc = mkAccountWithBalance(10.usd -> usd, 10.waves -> Waves)
        val wsc = mkWsAddressConnection(acc)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()

        val bo = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0)

        placeAndAwaitAtDex(bo)
        placeAndAwaitAtNode(mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 1.0))

        assertChanges(wsc)(
          Map(usd   -> WsBalances(0, 10), Waves -> WsBalances(9.997, 0.003)),
          Map(Waves -> WsBalances(9.997, 0)),
          Map(usd   -> WsBalances(0, 0)),
          Map(Waves -> WsBalances(19.997, 0)) // since balance increasing comes after transaction mining, + 10 - 0.003, Waves balance on Node = 19.997
        )(
          WsOrder.fromDomain(LimitOrder(bo)),
          WsOrder(bo.id(), status = OrderStatus.Filled.name, filledAmount = 10.0, filledFee = 0.003, avgWeighedPrice = 1.0)
        )

        wsc.close()
        dex1.api.cancelAll(acc)
      }

      "when user's order is partially filled with another one" in {

        val acc = mkAccountWithBalance(10.usd -> usd, 10.waves -> Waves)
        val wsc = mkWsAddressConnection(acc)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()

        val bo         = mkOrderDP(acc, wavesUsdPair, BUY, 10.waves, 1.0)
        val limitOrder = LimitOrder(bo)

        placeAndAwaitAtDex(bo)
        placeAndAwaitAtNode(mkOrderDP(alice, wavesUsdPair, SELL, 5.waves, 1.0))

        eventually {
          wsc.balanceChanges.squashed should matchTo(
            Map(
              usd   -> WsBalances(0, 5),
              Waves -> WsBalances(14.997, 0.0015) // since balance increasing comes after transaction mining, + 5 - 0.0015, Waves balance on Node = 14.9985
            )
          )

          wsc.orderChanges.squashed should matchTo(
            Map(
              limitOrder.id -> WsOrder
                .fromDomain(limitOrder)
                .copy(
                  id = limitOrder.id,
                  status = OrderStatus.PartiallyFilled.name.some,
                  filledAmount = 5.0.some,
                  filledFee = 0.0015.some,
                  avgWeighedPrice = 1.0.some
                )
            ))
        }
        wsc.clearMessages()

        dex1.api.cancelAll(acc)

        eventually {
          wsc.balanceChanges.squashed should matchTo(Map(usd -> WsBalances(5, 0), Waves -> WsBalances(14.9985, 0)))
          wsc.orderChanges.squashed should matchTo(
            Map(limitOrder.id -> WsOrder(bo.id(), status = OrderStatus.Cancelled.name))
          )
        }

        wsc.close()
      }

      "when user make a transfer" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 10.usd -> usd)
        val wsc = mkWsAddressConnection(acc)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()
        broadcastAndAwait(mkTransfer(acc, alice.toAddress, 2.usd, usd, feeAmount = 1.waves))
        assertChanges(wsc) { Map(Waves -> WsBalances(9, 0), usd -> WsBalances(8, 0)) }()

        wsc.close()
      }

      "user issued a new asset after establishing the connection" in {

        val acc = mkAccountWithBalance(10.waves -> Waves)
        val wsc = mkWsAddressConnection(acc)

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

        val wsc = mkWsAddressConnection(acc)
        assertChanges(wsc)(
          Map(Waves       -> WsBalances(9, 0)),
          Map(issuedAsset -> WsBalances(1000, 0))
        )()

        wsc.close()
      }

      "user burnt part of the asset amount" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
        val wsc = mkWsAddressConnection(acc)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(20, 0)) }()
        broadcastAndAwait(mkBurn(acc, usd, 10.usd))

        assertChanges(wsc)(
          Map(Waves -> WsBalances(9, 0)),
          Map(usd   -> WsBalances(10, 0))
        )()

        wsc.close()
      }

      "user burnt all of the asset amount" in {

        val acc = mkAccountWithBalance(10.waves -> Waves, 20.usd -> usd)
        val wsc = mkWsAddressConnection(acc)

        assertChanges(wsc, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(20, 0)) }()
        broadcastAndAwait(mkBurn(acc, usd, 20.usd))

        assertChanges(wsc)(
          Map(Waves -> WsBalances(9, 0)),
          Map(usd   -> WsBalances(0, 0))
        )()

        wsc.close()
      }
    }
  }

  "Second connection should get the actual data" in {

    val acc  = mkAccountWithBalance(500.usd -> usd, 10.waves -> Waves)
    val wsc1 = mkWsAddressConnection(acc, dex1)

    assertChanges(wsc1, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(500, 0)) }()

    val now = System.currentTimeMillis()

    val bo1 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0, ts = now)
    val bo2 = mkOrderDP(acc, wavesUsdPair, BUY, 100.waves, 1.0, ts = now + 1)

    Seq(bo1, bo2).foreach { o =>
      placeAndAwaitAtDex(o)
      Thread.sleep(150)
    }

    assertChanges(wsc1)(
      Map(usd -> WsBalances(400, 100), Waves -> WsBalances(9.997, 0.003)),
      Map(usd -> WsBalances(300, 200), Waves -> WsBalances(9.994, 0.006))
    )(
      WsOrder.fromDomain(LimitOrder(bo1)),
      WsOrder.fromDomain(LimitOrder(bo2))
    )

    val wsc2 = mkWsAddressConnection(acc, dex1)

    assertChanges(wsc2) { Map(Waves -> WsBalances(9.994, 0.006), usd -> WsBalances(300, 200)) }(
      WsOrder.fromDomain(LimitOrder(bo1)),
      WsOrder.fromDomain(LimitOrder(bo2))
    )

    Seq(wsc1, wsc2).foreach { _.close() }
    dex1.api.cancelAll(acc)
  }

  "Zero balances should not be in initial message" in {
    val acc  = mkAccountWithBalance(10.waves -> Waves, 10.usd -> usd)
    val wsc1 = mkWsAddressConnection(acc)

    assertChanges(wsc1, squash = false) { Map(Waves -> WsBalances(10, 0), usd -> WsBalances(10, 0)) }()

    broadcastAndAwait(mkBurn(acc, usd, 10.usd))
    assertChanges(mkWsAddressConnection(acc), squash = false) { Map(Waves -> WsBalances(9, 0)) }()

    broadcastAndAwait(mkTransfer(alice, acc, 5.usd, usd, 1.waves))
    assertChanges(mkWsAddressConnection(acc), squash = false) { Map(Waves -> WsBalances(9, 0), usd -> WsBalances(5, 0)) }()

    broadcastAndAwait(mkTransfer(acc, alice, 5.usd, usd, 1.waves))
    assertChanges(mkWsAddressConnection(acc), squash = false) { Map(Waves -> WsBalances(8, 0)) }()
  }

  "Subscription should be cancelled after jwt expiration" in {

    val acc = mkAccountWithBalance(10.waves -> Waves)
    val wsc = mkWsAddressConnection(acc, dex1, subscriptionLifetime = 3.seconds)

    wsc.receiveAtLeastN[WsAddressChanges](1) // snapshot
    wsc.receiveAtLeastN[WsError](1).head should matchTo(
      WsError(
        0, // ignored
        110105088, // SubscriptionTokenExpired
        s"The subscription token for address ${acc.toAddress} expired"
      )
    )

    wsc.isClosed shouldBe false

    Seq(3.seconds, 1.hour).foreach { subscriptionLifetime =>
      val jwt = mkJwt(acc, lifetime = subscriptionLifetime)
      wsc.send(WsAddressSubscribe(acc.toAddress, WsAddressSubscribe.defaultAuthType, jwt))
    }

    wsc.receiveAtLeastN[WsAddressChanges](1) // snapshot
    wsc.receiveNoMessages(3.5.seconds)

    wsc.close()
  }

  "Connection should close old address subscriptions when address subscriptions limit has been reached" in {
    val wsc = mkDexWsConnection(dex1)

    val carol = mkKeyPair("carol")
    val eve   = mkKeyPair("eve")

    Seq(alice, bob, carol, eve, alice).foreach { keyPair =>
      wsc.send(WsAddressSubscribe(keyPair, WsAddressSubscribe.defaultAuthType, mkJwt(keyPair)))
      wsc.receiveAtLeastN[WsAddressChanges](1)
    }

    wsc.receiveAtLeastN[WsError](2) should matchTo {
      List(
        WsError.from(SubscriptionsLimitReached(3, alice.toAddress.toString), 0L),
        WsError.from(SubscriptionsLimitReached(3, bob.toAddress.toString), 0L)
      )
    }
  }

  "Bugs" - {
    "DEX-816 Failure of AddressActor" in {
      dex1.stopWithoutRemove()
      broadcastAndAwait(IssueWctTx)
      dex1.start()

      val wsc = mkDexWsConnection(dex1)
      wsc.send(WsAddressSubscribe(bob, WsAddressSubscribe.defaultAuthType, mkJwt(bob)))

      eventually {
        wsc.receiveAtLeastN[WsAddressChanges](1)
      }
    }

    "DEX-817 Invalid WAVES balance after connection (leasing)" in {
      val bobWavesBalanceBefore = dex1.api.tradableBalance(bob, wavesBtcPair)(Waves)

      dex1.stopWithoutRemove()
      val leaseTx = mkLease(bob, alice, bobWavesBalanceBefore - 0.1.waves, fee = leasingFee)
      broadcastAndAwait(leaseTx)
      dex1.start()

      val wsc = mkDexWsConnection(dex1)
      wsc.send(WsAddressSubscribe(bob, WsAddressSubscribe.defaultAuthType, mkJwt(bob)))

      eventually {
        val balance = wsc.receiveAtLeastN[WsAddressChanges](1).map(_.balances).squashed - btc - wct
        balance should matchTo(
          Map[Asset, WsBalances](
            Waves -> WsBalances(Denormalization.denormalizeAmountAndFee(0.1.waves - leasingFee, 8).toDouble, 0)
          ))
      }

      broadcastAndAwait(mkLeaseCancel(bob, leaseTx.getId))
    }

    "DEX-818" - {
      "Connections can affect each other" in {
        val wscs    = (1 to 10).map(_ => mkWsAddressConnection(bob))
        val mainWsc = mkWsAddressConnection(bob)

        markup("Multiple orders")
        val now = System.currentTimeMillis()
        val orders = (1 to 50).map { i =>
          mkOrderDP(bob, wavesBtcPair, BUY, 1.waves, 0.00012, ts = now + i)
        }

        Await.result(Future.traverse(orders)(dex1.asyncApi.place), 1.minute)
        dex1.api.cancelAll(bob)

        wscs.par.foreach(_.close())
        Thread.sleep(3000)
        mainWsc.clearMessages()

        markup("A new order")
        placeAndAwaitAtDex(mkOrderDP(bob, wavesBtcPair, BUY, 2.waves, 0.00029))

        eventually {
          mainWsc.receiveAtLeastN[WsAddressChanges](1)
        }
        mainWsc.clearMessages()
      }

      "Negative balances" in {
        val carol = mkAccountWithBalance(5.waves -> Waves)
        val wsc   = mkWsAddressConnection(carol)

        val now = System.currentTimeMillis()
        val txs = (1 to 2).map { i =>
          mkTransfer(carol, alice, 5.waves - minFee, Waves, minFee, timestamp = now + i)
        }
        val simulation = Future.traverse(txs)(wavesNode1.asyncApi.broadcast(_))
        Await.result(simulation, 1.minute)
        wavesNode1.api.waitForHeightArise()

        wsc.balanceChanges.zipWithIndex.foreach {
          case (changes, i) =>
            changes.foreach {
              case (asset, balance) =>
                withClue(s"$i: $asset -> $balance: ") {
                  balance.tradable should be >= 0.0
                  balance.reserved should be >= 0.0
                }
            }
        }
      }
    }

    "DEX-827 Wrong balance" in {
      val btcBalance = 461
      val carol      = mkAccountWithBalance(25.waves -> Waves, btcBalance.btc -> btc)
      val wsc        = mkWsAddressConnection(carol)

      val now    = System.currentTimeMillis()
      val order1 = mkOrderDP(carol, wavesBtcPair, BUY, 4.7.waves, 6, matcherFee = 0.003.waves, ts = now + 1)
      val order2 = mkOrderDP(carol, wavesBtcPair, BUY, 4.7.waves, 6, matcherFee = 0.003.waves, ts = now + 2)
      val order3 = mkOrderDP(carol, wavesBtcPair, SELL, 10.waves, 6, matcherFee = 0.003.waves)

      dex1.api.place(order1)
      dex1.api.place(order2)

      placeAndAwaitAtDex(order3, HttpOrderStatus.Status.PartiallyFilled)
      dex1.api.cancelAll(carol)

      waitForOrderAtNode(order1)
      waitForOrderAtNode(order2)
      waitForOrderAtNode(order3)

      wavesNode1.api.waitForHeightArise()

      val expectedWavesBalance = 25.0 - 0.003 * 2 - 0.003 * 4.7 * 2 / 10

      wavesNode1.api.balance(carol, Waves) shouldBe expectedWavesBalance.waves
      wavesNode1.api.balance(carol, btc) shouldBe btcBalance.btc

      dex1.api.tradableBalance(carol, wavesBtcPair) should matchTo(
        Map(
          Waves -> expectedWavesBalance.waves,
          btc   -> btcBalance.btc
        )
      )

      wsc.balanceChanges.squashed should matchTo(
        Map(
          Waves -> WsBalances(expectedWavesBalance, 0),
          btc   -> WsBalances(btcBalance, 0)
        ))
    }

    "DEX-828 Excess data in snapshots" in {

      val initialBalance: (Long, Asset)                         = 10.waves -> Waves
      val expectedBalanceSnapshot: List[Map[Asset, WsBalances]] = List(Map[Asset, WsBalances](Waves -> WsBalances(10, 0)))

      def getBalanceSnapshot(account: KeyPair): List[Map[Asset, WsBalances]] =
        mkWsAddressConnection(account).receiveAtLeastN[WsAddressChanges](1).map(_.balances)

      // with tradable balance request
      val carol = mkAccountWithBalance(initialBalance)
      dex1.api.tradableBalance(carol, wavesUsdPair)
      getBalanceSnapshot(carol) should matchTo(expectedBalanceSnapshot)

      // without tradable balance request
      val eve = mkAccountWithBalance(initialBalance)
      getBalanceSnapshot(eve) should matchTo(expectedBalanceSnapshot)
    }
  }
}
