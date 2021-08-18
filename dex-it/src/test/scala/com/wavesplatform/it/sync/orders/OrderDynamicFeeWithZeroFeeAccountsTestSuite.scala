package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.AcceptedOrder
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

import java.util.concurrent.atomic.AtomicLong

final class OrderDynamicFeeWithZeroFeeAccountsTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {

  "OrderDynamicFeeWithZeroFeeAccountsTestSuite" - {

    "should match two orders with default fee for usual accounts" in test { ib =>
      val aliceOrder = mkAliceOrder(10.waves, 1.usd, OrderType.BUY)
      val bobOrder = mkBobOrder(10.waves, 1.usd, OrderType.SELL)
      placeAndAwaitAtDex(aliceOrder)
      placeAndAwaitAtDex(bobOrder, Status.Filled)

      eventually {
        wavesNode1.api.wavesBalance(alice) shouldBe ib.aliceWavesBalance + 10.waves - matcherFee
        wavesNode1.api.wavesBalance(bob) shouldBe ib.bobWavesBalance - 10.waves - matcherFee
        val expectedUsd = AcceptedOrder.calcAmountOfPriceAsset(10.waves, 1.usd)
        wavesNode1.api.balance(alice, usd) shouldBe ib.aliceUsdBalance - expectedUsd
        wavesNode1.api.balance(bob, usd) shouldBe ib.bobUsdBalance + expectedUsd
        wavesNode1.api.wavesBalance(matcher) shouldBe ib.matcherWavesBalance + 2 * matcherFee - tradeFee
      }
    }

    "should return reserved balance based on order for zero fee accounts" in {
      restartWithNewZeroFeeAccounts(Set(alice))
      val aliceOrder = mkAliceOrder(10.waves, 1.usd, OrderType.SELL)
      placeAndAwaitAtDex(aliceOrder)
      dex1.api.getReservedBalanceWithApiKey(alice).get(Waves).value shouldBe 10.waves + matcherFee
      dex1.api.cancelOrderById(aliceOrder)
      eventually {
        dex1.api.getReservedBalanceWithApiKey(alice) shouldBe empty
      }
    }

    "should match two orders with fee in waves" in {
      forAll(Table(
        ("maker", "taker"),
        (mkAliceOrder(10.waves, 1.usd, OrderType.BUY), mkBobOrder(10.waves, 1.usd, OrderType.SELL)),
        (mkBobOrder(10.waves, 1.usd, OrderType.SELL), mkAliceOrder(10.waves, 1.usd, OrderType.BUY))
      )) { (makerOrder, takerOrder) =>
        test { ib =>
          placeAndAwaitAtDex(makerOrder)
          placeAndAwaitAtDex(takerOrder, Status.Filled)

          eventually {
            wavesNode1.api.wavesBalance(alice) shouldBe ib.aliceWavesBalance + 10.waves
            wavesNode1.api.wavesBalance(bob) shouldBe ib.bobWavesBalance - 10.waves - matcherFee
            val expectedUsd = AcceptedOrder.calcAmountOfPriceAsset(10.waves, 1.usd)
            wavesNode1.api.balance(alice, usd) shouldBe ib.aliceUsdBalance - expectedUsd
            wavesNode1.api.balance(bob, usd) shouldBe ib.bobUsdBalance + expectedUsd
            wavesNode1.api.wavesBalance(matcher) shouldBe ib.matcherWavesBalance + matcherFee - tradeFee
          }
        }
      }
    }

    "should match two orders with fee in usd" in {
      //with this rate minimal required matcherFee should be 15 (50 * 0.003.waves * 10^-6)
      upsertAssetRate(usd -> 50)
      val usdFee = 15
      forAll(Table(
        ("maker", "taker"),
        (
          mkAliceOrder(10.waves, 1.usd, OrderType.BUY, feeAsset = usd, matcherFee = usdFee),
          mkBobOrder(10.waves, 1.usd, OrderType.SELL, feeAsset = usd, matcherFee = usdFee)
        ),
        (
          mkBobOrder(10.waves, 1.usd, OrderType.SELL, feeAsset = usd, matcherFee = usdFee),
          mkAliceOrder(10.waves, 1.usd, OrderType.BUY, feeAsset = usd, matcherFee = usdFee)
        )
      )) { (makerOrder, takerOrder) =>
        test { ib =>
          placeAndAwaitAtDex(makerOrder)
          placeAndAwaitAtDex(takerOrder, Status.Filled)

          eventually {
            wavesNode1.api.wavesBalance(alice) shouldBe ib.aliceWavesBalance + 10.waves
            wavesNode1.api.wavesBalance(bob) shouldBe ib.bobWavesBalance - 10.waves
            val expectedUsd = AcceptedOrder.calcAmountOfPriceAsset(10.waves, 1.usd)
            wavesNode1.api.balance(alice, usd) shouldBe ib.aliceUsdBalance - expectedUsd
            wavesNode1.api.balance(bob, usd) shouldBe ib.bobUsdBalance + expectedUsd - usdFee
            wavesNode1.api.wavesBalance(matcher) shouldBe ib.matcherWavesBalance - tradeFee
            wavesNode1.api.balance(matcher, usd) shouldBe ib.matcherUsdBalance + usdFee
          }
        }
      }
    }

    "should match several orders with large market order placed by zero fee account" in test { ib =>
      List(mkBobOrder(10.waves, 1.usd, OrderType.SELL), mkBobOrder(10.waves, 1.usd, OrderType.SELL))
        .foreach(placeAndAwaitAtDex(_))
      placeAndAwaitAtDex(mkAliceOrder(20.waves, 1.usd, OrderType.BUY), Status.Filled, isMarketOrder = true)

      eventually {
        wavesNode1.api.wavesBalance(alice) shouldBe ib.aliceWavesBalance + 20.waves
        wavesNode1.api.wavesBalance(bob) shouldBe ib.bobWavesBalance - 20.waves - 2 * matcherFee
        val expectedUsd = AcceptedOrder.calcAmountOfPriceAsset(20.waves, 1.usd)
        wavesNode1.api.balance(alice, usd) shouldBe ib.aliceUsdBalance - expectedUsd
        wavesNode1.api.balance(bob, usd) shouldBe ib.bobUsdBalance + expectedUsd
        wavesNode1.api.wavesBalance(matcher) shouldBe ib.matcherWavesBalance + 2 * matcherFee - 2 * tradeFee
      }
    }

    "should match several orders with large limit order placed by zero fee account" in {
      forAll(Table(
        ("maker", "taker"),
        (
          List(mkAliceOrder(20.waves, 1.usd, OrderType.BUY)),
          List(mkBobOrder(10.waves, 1.usd, OrderType.SELL), mkBobOrder(10.waves, 1.usd, OrderType.SELL))
        ),
        (
          List(mkBobOrder(10.waves, 1.usd, OrderType.SELL), mkBobOrder(10.waves, 1.usd, OrderType.SELL)),
          List(mkAliceOrder(20.waves, 1.usd, OrderType.BUY))
        )
      )) { (makerOrders, takerOrders) =>
        test { ib =>
          makerOrders.foreach(placeAndAwaitAtDex(_))
          takerOrders.foreach(placeAndAwaitAtDex(_, Status.Filled))

          eventually {
            wavesNode1.api.wavesBalance(alice) shouldBe ib.aliceWavesBalance + 20.waves
            wavesNode1.api.wavesBalance(bob) shouldBe ib.bobWavesBalance - 20.waves - 2 * matcherFee
            val expectedUsd = AcceptedOrder.calcAmountOfPriceAsset(20.waves, 1.usd)
            wavesNode1.api.balance(alice, usd) shouldBe ib.aliceUsdBalance - expectedUsd
            wavesNode1.api.balance(bob, usd) shouldBe ib.bobUsdBalance + expectedUsd
            wavesNode1.api.wavesBalance(matcher) shouldBe ib.matcherWavesBalance + 2 * matcherFee - 2 * tradeFee
          }
        }
      }
    }

    "should take zero fees for zero fee accounts while trading with self" in test { ib =>
      placeAndAwaitAtDex(mkAliceOrder(20.waves, 1.usd, OrderType.BUY))
      placeAndAwaitAtDex(mkAliceOrder(20.waves, 1.usd, OrderType.SELL), Status.Filled)
      eventually {
        wavesNode1.api.wavesBalance(alice) shouldBe ib.aliceWavesBalance
        wavesNode1.api.balance(alice, usd) shouldBe ib.aliceUsdBalance
        wavesNode1.api.wavesBalance(matcher) shouldBe ib.matcherWavesBalance - tradeFee
      }
    }

  }

  private val ts = new AtomicLong(System.currentTimeMillis())

  private def mkAliceOrder(amount: Long, price: Long, orderType: OrderType, feeAsset: Asset = Waves, matcherFee: Long = matcherFee): Order =
    mkOrder(alice, wavesUsdPair, orderType, amount, price, matcherFee, feeAsset, ts.incrementAndGet())

  def mkBobOrder(amount: Long, price: Long, orderType: OrderType, feeAsset: Asset = Waves, matcherFee: Long = matcherFee): Order =
    mkOrder(bob, wavesUsdPair, orderType, amount, price, matcherFee, feeAsset, ts.incrementAndGet())

  private def upsertAssetRate(pairs: (IssuedAsset, Double)*): Unit = pairs.foreach {
    case (asset, rate) => withClue(s"$asset")(dex1.api.upsertAssetRate(asset, rate))
  }

  private def restartWithNewZeroFeeAccounts(zeroFeeAccounts: Set[PublicKey]): Unit = {
    val newConfig = ConfigFactory.parseString(
      s"""
         |waves.dex.order-fee.-1.dynamic {
         |  zero-fee-accounts = [${zeroFeeAccounts.map(x => s""" "${x.base58}" """.trim).mkString(",")}]
         |}
         |""".stripMargin
    ).withFallback(dexInitialConf)
    dex1.restartWithNewSuiteConfig(newConfig)
  }

  private val dexInitialConf = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee.-1 {
       |    mode = dynamic
       |    dynamic {
       |      base-maker-fee = $matcherFee
       |      base-taker-fee = $matcherFee
       |      zero-fee-accounts = []
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def dexInitialSuiteConfig: Config = dexInitialConf

  private def test[A](f: InitialBalances => A): Unit = {
    val matcherWavesBalance = wavesNode1.api.wavesBalance(matcher)
    val matcherUsdBalance = wavesNode1.api.balance(matcher, usd)
    val bobWavesBalance = wavesNode1.api.wavesBalance(bob)
    val bobUsdBalance = wavesNode1.api.balance(bob, usd)
    val aliceWavesBalance = wavesNode1.api.wavesBalance(alice)
    val aliceUsdBalance = wavesNode1.api.balance(alice, usd)
    f(InitialBalances(matcherWavesBalance, matcherUsdBalance, bobWavesBalance, bobUsdBalance, aliceWavesBalance, aliceUsdBalance))
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    broadcastAndAwait(
      mkTransfer(alice, bob, 500_000.usd, usd),
      mkTransfer(alice, matcher, 500_000.usd, usd)
    )
    dex1.start()
  }

  private case class InitialBalances(
    matcherWavesBalance: Long,
    matcherUsdBalance: Long,
    bobWavesBalance: Long,
    bobUsdBalance: Long,
    aliceWavesBalance: Long,
    aliceUsdBalance: Long
  )

}
