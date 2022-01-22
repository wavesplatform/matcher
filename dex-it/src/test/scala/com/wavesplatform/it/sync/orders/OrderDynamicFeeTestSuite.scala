package com.wavesplatform.it.sync.orders

import cats.instances.long._
import cats.instances.map._
import cats.syntax.semigroup._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpV0LevelAgg
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.{FeeNotEnough, UnexpectedFeeAsset}
import com.wavesplatform.dex.model.AcceptedOrder

// TODO refactor balances retrieving
class OrderDynamicFeeTestSuite extends OrderFeeBaseTestSuite {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      default {
       |        mode = dynamic
       |        dynamic {
       |          base-maker-fee = $matcherFee
       |          base-taker-fee = $matcherFee
       |        }
       |        percent {
       |          asset-type = amount
       |          min-fee = 10
       |          min-fee-in-waves = $percentMinFeeInWaves
       |        }
       |        fixed {
       |          asset = "$EthId"
       |          min-fee = 10
       |        }
       |      }
       |      discount {
       |        asset = "$BtcId"
       |        value = 0
       |      }
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueWctTx, IssueUsdTx, IssueEthTx, IssueBtcTx)
    broadcastAndAwait(mkTransfer(bob, alice, 100.btc, btc))
    broadcastAndAwait(mkTransfer(alice, bob, 100.eth, eth))
    dex1.start()
  }

  def mkBobOrder(feeAsset: Asset, fee: Long): Order = mkOrder(
    owner = bob,
    pair = wavesBtcPair,
    orderType = OrderType.BUY,
    amount = 1.waves,
    price = 50000L,
    matcherFee = fee,
    version = 3,
    feeAsset = feeAsset
  )

  def mkAliceOrder(feeAsset: Asset, fee: Long): Order = mkOrder(
    owner = alice,
    pair = wavesBtcPair,
    orderType = OrderType.SELL,
    amount = 1.waves,
    price = 50000L,
    matcherFee = fee,
    version = 3,
    feeAsset = feeAsset
  )

  "dynamic fee settings with different assets" - {
    val btcRate = 0.001
    val ethRate = 0.01

    "fee is not enough" in {
      upsertAssetRate(btc -> btcRate, eth -> ethRate)
      dex1.tryApi.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 100L,
          feeAsset = btc
        )
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.000003 $BtcId as fee for this order, but given 0.000001 $BtcId"
      )
    }

    "fee is enough" in {
      dex1.api.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 300L,
          feeAsset = btc
        )
      )

      val before = dex1.api.getReservedBalanceWithApiKey(bob)
      before.keys should contain(btc)
      before(Waves) shouldEqual 100000000L
      dex1.api.cancelAllOrdersWithSig(bob)

      dex1.api.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 300L,
          version = 3,
          feeAsset = btc
        )
      )
      val after = dex1.api.getReservedBalanceWithApiKey(bob)
      after(btc) shouldEqual 50300L
      after.keys shouldNot contain(Waves)
      dex1.api.cancelAllOrdersWithSig(bob)
    }

    "have proper balances after matching" in {
      def runTest(feeAsset: Asset, matcherFee: Long): Unit = {
        val bobEthBalance = wavesNode1.api.balance(bob, eth)
        val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
        val bobFeeBalance = wavesNode1.api.balance(bob, feeAsset)
        val aliceEthBalance = wavesNode1.api.balance(alice, eth)
        val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
        val aliceFeeBalance = wavesNode1.api.balance(alice, feeAsset)
        val matcherFeeAssetBalance = wavesNode1.api.balance(matcher, feeAsset)

        val bobOrder = mkOrder(
          owner = bob,
          pair = ethWavesPair,
          orderType = OrderType.BUY,
          amount = 2.5.btc,
          price = 1.waves,
          matcherFee = matcherFee,
          feeAsset = feeAsset
        )
        dex1.api.place(bobOrder)
        dex1.api.place(
          mkOrder(
            owner = alice,
            pair = ethWavesPair,
            orderType = OrderType.SELL,
            amount = 2.5.btc,
            price = 1.waves,
            matcherFee = matcherFee,
            feeAsset = feeAsset
          )
        )
        waitForOrderAtNode(bobOrder)
        eventually {
          def check(account: KeyPair, wavesBalance: Long, ethBalance: Long, feeBalance: Long, isBid: Boolean) = {
            val actual =
              Map(
                eth -> wavesNode1.api.balance(account, eth),
                Waves -> wavesNode1.api.balance(account, Waves),
                feeAsset -> wavesNode1.api.balance(account, feeAsset)
              )
            val expected =
              Map(eth -> ethBalance, Waves -> wavesBalance, feeAsset -> feeBalance) |+|
              Map(
                eth -> AcceptedOrder.correctedAmountOfAmountAsset(2.5.btc, 1.waves) * (if (isBid) 1 else -1),
                Waves -> AcceptedOrder.calcAmountOfPriceAsset(2.5.btc, 1.waves) * (if (isBid) -1 else 1)
              ) |+| Map(feeAsset -> -matcherFee)

            actual shouldBe expected
          }

          check(bob, bobWavesBalance, bobEthBalance, bobFeeBalance, isBid = true)
          check(alice, aliceWavesBalance, aliceEthBalance, aliceFeeBalance, isBid = false)
          wavesNode1.api.balance(matcher, feeAsset) shouldBe
          matcherFeeAssetBalance + 2 * matcherFee - (if (feeAsset == Waves) 0.003.waves else 0L)
        }
      }

      writeGlobalLog(s"feeAsset = Btc ($BtcId)")
      runTest(btc, 300L)

      dex1.safeRestartWithNewSuiteConfig(
        ConfigFactory.parseString(
          s""" waves.dex.order-fee.-1.composite.discount.asset="$EthId" """
        ).withFallback(dexInitialSuiteConfig)
      )

      writeGlobalLog(s"feeAsset = Eth ($EthId)")
      runTest(eth, 3000L)

      writeGlobalLog(s"feeAsset = Waves")
      runTest(Waves, 0.003.waves)
    }
  }

  "asset fee is not supported" - {
    val btcRate = 0.001
    val ethRate = 0.01

    "only waves, eth (as discount asset) are allowed" in {
      upsertAssetRate(btc -> btcRate, eth -> ethRate)

      dex1.tryApi.place(mkBobOrder(btc, 300L)) should failWith(
        UnexpectedFeeAsset.code,
        s"Required one of the following fee asset: WAVES, $EthId. But given $BtcId"
      )
    }

    "asset became not supported after order was placed" in {
      val bobEthBalance = wavesNode1.api.balance(bob, eth)
      val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)

      val bobOrder = mkBobOrder(eth, 3000L)
      dex1.api.place(bobOrder)
      dex1.api.deleteAssetRate(eth)
      dex1.api.place(mkAliceOrder(Waves, 0.003.waves))
      dex1.api.waitForOrderStatus(bobOrder, Status.Filled)
      waitForOrderAtNode(bobOrder)

      eventually {
        wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 3000L)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 0.003.waves - 1.waves)
      }

    }

    "asset became not supported after order was partially filled" in {
      upsertAssetRate(eth -> ethRate)

      val bobEthBalance = wavesNode1.api.balance(bob, eth)
      val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
      val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)

      val aliceOrder = mkOrder(
        owner = alice,
        matcher = matcher,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 3000L,
        feeAsset = eth
      )

      dex1.api.place(aliceOrder)
      dex1.api.getReservedBalanceWithApiKey(alice)(eth) shouldBe 3000L
      dex1.api.place(mkBobOrder(eth, 3000L))
      dex1.api.waitForOrderStatus(aliceOrder, Status.PartiallyFilled)

      waitForOrderAtNode(aliceOrder)

      eventually {
        dex1.api.getReservedBalanceWithApiKey(alice)(eth) shouldBe 1500L
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 1500L)
        wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 3000L)
        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
      }

      dex1.api.deleteAssetRate(eth)

      val bobSecondOrder = mkBobOrder(Waves, 0.003.waves)

      dex1.api.place(bobSecondOrder)
      dex1.api.waitForOrderStatus(aliceOrder, Status.Filled)

      waitForOrderAtNode(bobSecondOrder)

      eventually {
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 2.waves)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 3000L)
        wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 3000L)
        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance - 0.003.waves + 2.waves)
      }
    }

    "rates of asset pair was changed while order is placed (reserved balance should stay the same)" in {
      upsertAssetRate(eth -> ethRate)
      val bobEthBalance = wavesNode1.api.balance(bob, eth)
      val bobOrder = mkBobOrder(eth, 3000L)
      dex1.api.place(bobOrder)

      dex1.api.getReservedBalanceWithApiKey(bob)(eth) shouldBe 3000L

      upsertAssetRate(eth -> ethRate * 2)

      dex1.api.getReservedBalanceWithApiKey(bob)(eth) shouldBe 3000L
      dex1.api.place(mkAliceOrder(eth, 3000L))

      waitForOrderAtNode(bobOrder)

      eventually(wavesNode1.api.balance(bob, eth) shouldBe bobEthBalance - 3000L)
    }
  }

  "orders with non-waves asset fee" - {
    val btcRate = 0.001
    val ethRate = 0.01

    "are full filled" in {
      upsertAssetRate(btc -> btcRate, eth -> ethRate)

      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
      val bobEthBalance = wavesNode1.api.balance(bob, eth)
      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)
      val matcherEthBalance = wavesNode1.api.balance(matcher, eth)

      val bobOrder = mkBobOrder(eth, 3000L)
      placeAndAwaitAtDex(bobOrder)
      dex1.api.getReservedBalanceWithApiKey(bob).keys should not contain Waves

      val aliceOrder = mkAliceOrder(eth, 3000L)
      dex1.api.place(aliceOrder)

      List(bobOrder, aliceOrder).foreach(dex1.api.waitForOrderStatus(_, Status.Filled))
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

      eventually {
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - AcceptedOrder.calcAmountOfPriceAsset(1.waves, 50000L))
        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + AcceptedOrder.correctedAmountOfAmountAsset(1.waves, 50000L))
        wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 3000L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + AcceptedOrder.calcAmountOfPriceAsset(1.waves, 50000L))
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - AcceptedOrder.correctedAmountOfAmountAsset(1.waves, 50000L))
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 3000L)
        wavesNode1.api.balance(matcher, eth) shouldBe (matcherEthBalance + 2 * 3000L)
      }
    }

    "are partially filled" in {
      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
      val bobEthBalance = wavesNode1.api.balance(bob, eth)
      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)
      val matcherEthBalance = wavesNode1.api.balance(matcher, eth)

      val bobOrder = mkBobOrder(eth, 3000L)
      dex1.api.place(bobOrder)

      val aliceOrder = mkOrder(
        owner = alice,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 3000L,
        feeAsset = eth
      )
      dex1.api.place(aliceOrder)

      Map(bobOrder -> Status.Filled, aliceOrder -> Status.PartiallyFilled).foreach(Function.tupled(dex1.api.waitForOrderStatus))
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

      eventually {
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - AcceptedOrder.calcAmountOfPriceAsset(1.waves, 50000L))
        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + AcceptedOrder.correctedAmountOfAmountAsset(1.waves, 50000L))
        wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 3000L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + AcceptedOrder.calcAmountOfPriceAsset(1.waves, 50000L))
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - AcceptedOrder.correctedAmountOfAmountAsset(1.waves, 50000L))
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 1500L)
        wavesNode1.api.balance(matcher, eth) shouldBe (matcherEthBalance + 3000L + 1500L)
      }

      dex1.api.cancelAllOrdersWithSig(alice)
    }
  }

  "cancellation of" - {
    val btcRate = 0.001
    val ethRate = 0.01

    "order with non-waves fee" in {
      upsertAssetRate(btc -> btcRate, eth -> ethRate)
      val bobBalance = dex1.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair)

      val order = mkBobOrder(eth, 3000L)

      dex1.api.place(order)
      dex1.api.cancelOneOrAllInPairOrdersWithSig(bob, order).status shouldBe "OrderCanceled"
      dex1.api.getReservedBalanceWithApiKey(bob).keys.size shouldBe 0
      dex1.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair) shouldEqual bobBalance
    }

    "partially filled order with non-waves fee" in {
      val aliceEthBalance = dex1.api.getTradableBalanceByAssetPairAndAddress(alice, ethWavesPair)(eth)

      val bobOrder = mkBobOrder(eth, 3000L)

      dex1.api.place(bobOrder)

      val aliceOrder = mkOrder(
        owner = alice,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 3920L,
        feeAsset = eth
      )

      dex1.api.place(aliceOrder)
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))
      dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, aliceOrder).status shouldBe "OrderCanceled"
      dex1.api.getReservedBalanceWithApiKey(alice).keys.size shouldBe 0
      wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 3920L / 2)
      List(btc, eth).foreach(dex1.api.deleteAssetRate)
    }
  }

  "fee in pairs with different decimals count" in {
    dex1.safeRestartWithNewSuiteConfig(
      ConfigFactory.parseString(
        s""" waves.dex.order-fee.-1.composite.discount.asset="$UsdId" """
      ).withFallback(dexInitialSuiteConfig)
    )
    upsertAssetRate(usd -> 5d)
    dex1.tryApi.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, matcherFee = 1L, feeAsset = usd)) should failWith(
      FeeNotEnough.code,
      s"Required 0.02 $UsdId as fee for this order, but given 0.01 $UsdId"
    )

    upsertAssetRate(usd -> 3)
    broadcastAndAwait(mkTransfer(alice, bob.toAddress, 1L, usd))

    val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
    val aliceUsdBalance = wavesNode1.api.balance(alice, usd)
    val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
    val bobUsdBalance = wavesNode1.api.balance(bob, usd)

    val bobOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, matcherFee = 1L, feeAsset = usd)
    dex1.api.place(bobOrder)

    dex1.api.getOrderBook(wavesUsdPair).asks shouldBe List(HttpV0LevelAgg(1.waves, 300))
    dex1.api.getReservedBalanceWithApiKey(bob) shouldBe Map(usd -> 1L, Waves -> 1.waves)
    dex1.api.cancelOneOrAllInPairOrdersWithSig(bob, bobOrder)

    wavesNode1.api.balance(alice, Waves) shouldBe aliceWavesBalance
    wavesNode1.api.balance(alice, usd) shouldBe aliceUsdBalance

    wavesNode1.api.balance(bob, Waves) shouldBe bobWavesBalance
    wavesNode1.api.balance(bob, usd) shouldBe bobUsdBalance

    val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300, matcherFee = 1L, feeAsset = usd)
    dex1.api.place(aliceOrderId)

    dex1.api.getOrderBook(wavesUsdPair).bids shouldBe List(HttpV0LevelAgg(1.waves, 300))
    dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(usd -> 301)

    dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, 1L, feeAsset = usd))

    waitForOrderAtNode(aliceOrderId)
    wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance + 1.waves)
    wavesNode1.api.balance(alice, usd) shouldBe (aliceUsdBalance - 301)

    wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance - 1.waves)
    wavesNode1.api.balance(bob, usd) shouldBe (bobUsdBalance + 299)

    dex1.api.deleteAssetRate(usd)
  }

  "rounding fee to filled amount" - {
    "if amount cannot be filled" in {

      Seq(wct, btc, usd).foreach(asset => upsertAssetRate(asset -> 0.000003d))

      withClue("price asset is fee asset") {

        dex1.safeRestartWithNewSuiteConfig(
          ConfigFactory.parseString(
            s""" waves.dex.order-fee.-1.composite.discount.asset="$WctId" """
          ).withFallback(dexInitialSuiteConfig)
        )

        val bobWctBalance = wavesNode1.api.balance(bob, wct)
        val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
        val bobUsdBalance = wavesNode1.api.balance(bob, usd)

        val bobOrderId = mkOrder(bob, wavesUsdPair, OrderType.SELL, 425532L, 238, 1, feeAsset = wct)
        val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 238, matcherFee, version = 3)

        dex1.api.place(bobOrderId)
        dex1.api.place(aliceOrderId)

        dex1.api.waitForOrderStatus(bobOrderId, Status.Filled)
        dex1.api.waitForOrderStatus(aliceOrderId, Status.PartiallyFilled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance - 420169L)
        wavesNode1.api.balance(bob, usd) shouldBe (bobUsdBalance + 1)
        wavesNode1.api.balance(bob, wct) shouldBe (bobWctBalance - 1)

        dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, aliceOrderId)
      }

      withClue("buy order") {
        broadcastAndAwait(mkTransfer(bob, alice, 1, wct, 0.001.waves))

        val aliceWctBalance = wavesNode1.api.balance(alice, wct)
        val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
        val aliceUsdBalance = wavesNode1.api.balance(alice, usd)

        val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 851064L, 238, 1, feeAsset = wct)
        val bobOrderId = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 238, matcherFee, version = 3)

        dex1.api.place(aliceOrderId)
        dex1.api.place(bobOrderId)

        dex1.api.waitForOrderStatus(aliceOrderId, Status.Filled)
        dex1.api.waitForOrderStatus(bobOrderId, Status.PartiallyFilled)

        waitForOrderAtNode(aliceOrderId)

        wavesNode1.api.balance(alice, wct) shouldBe (aliceWctBalance - 1)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance + 840337L)
        wavesNode1.api.balance(alice, usd) shouldBe (aliceUsdBalance - 2)

        dex1.api.cancelOneOrAllInPairOrdersWithSig(bob, bobOrderId)
      }

      withClue("price asset is not fee asset") {

        dex1.safeRestartWithNewSuiteConfig(
          ConfigFactory.parseString(
            s""" waves.dex.order-fee.-1.composite.discount.asset="$usd" """
          ).withFallback(dexInitialSuiteConfig)
        )

        val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
        val bobUsdBalance = wavesNode1.api.balance(bob, usd)

        val bobOrderId = mkOrder(bob, wavesUsdPair, OrderType.SELL, 851064L, 238, 1, feeAsset = usd)
        val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 238, matcherFee, version = 3)

        dex1.api.place(bobOrderId)
        dex1.api.place(aliceOrderId)

        dex1.api.waitForOrderStatus(bobOrderId, Status.Filled)
        dex1.api.waitForOrderStatus(aliceOrderId, Status.PartiallyFilled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance - 840337L)
        wavesNode1.api.balance(bob, usd) shouldBe (bobUsdBalance + 1)

        dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, aliceOrderId)
      }

      Seq(wct, btc, usd).foreach(dex1.api.deleteAssetRate)
    }

    "if v2 order filled partially by too low percent of amount" in {

      val aliceWavesBefore = wavesNode1.api.balance(alice.toAddress, Waves)
      val bobWavesBefore = wavesNode1.api.balance(bob.toAddress, Waves)

      val buyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1000000000.waves, 100, 0.003.waves, version = 2: Byte)

      placeAndAwaitAtDex(buyOrder)

      val sellOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 100, 0.003.waves, version = 2: Byte)

      placeAndAwaitAtDex(sellOrder, Status.Filled)
      waitForOrderAtNode(sellOrder)

      dex1.api.waitForOrderStatus(buyOrder, Status.PartiallyFilled).filledAmount shouldBe Some(1.waves)

      wavesNode1.api.balance(alice.toAddress, Waves) should be(aliceWavesBefore + 1.waves)
      wavesNode1.api.balance(bob.toAddress, Waves) should be(bobWavesBefore - 1.waves - 0.003.waves)

      dex1.api.cancelAllOrdersWithSig(alice)
    }

    "if v3 order filled partially by too low percent of amount" in {

      val aliceWavesBefore = wavesNode1.api.balance(alice.toAddress, Waves)
      val bobWavesBefore = wavesNode1.api.balance(bob.toAddress, Waves)

      val buyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1000000000.waves, 100, 0.003.waves, version = 3: Byte)

      placeAndAwaitAtDex(buyOrder)

      val sellOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 100, 0.003.waves, version = 3: Byte)

      placeAndAwaitAtDex(sellOrder, Status.Filled)
      waitForOrderAtNode(sellOrder)

      dex1.api.waitForOrderStatus(buyOrder, Status.PartiallyFilled).filledAmount shouldBe Some(1.waves)

      wavesNode1.api.balance(alice.toAddress, Waves) should be(aliceWavesBefore + 1.waves - 1)
      wavesNode1.api.balance(bob.toAddress, Waves) should be(bobWavesBefore - 1.waves - 0.003.waves)

      dex1.api.cancelAllOrdersWithSig(alice)
    }

    "if order was filled partially" in {

      Seq(btc, usd).foreach(asset => upsertAssetRate(asset -> 0.000003d))

      dex1.safeRestartWithNewSuiteConfig(
        ConfigFactory.parseString(
          s""" waves.dex.order-fee.-1.composite.discount.asset="$BtcId" """
        ).withFallback(dexInitialSuiteConfig)
      )

      withClue("price asset is fee asset") {

        val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
        val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)

        val aliceOrderId = mkOrder(alice, wavesBtcPair, OrderType.SELL, 100.waves, 10591, 1, feeAsset = btc)
        val bobOrderId = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)

        dex1.api.place(aliceOrderId)
        dex1.api.place(bobOrderId)

        dex1.api.waitForOrderStatus(aliceOrderId, Status.PartiallyFilled)
        dex1.api.waitForOrderStatus(bobOrderId, Status.Filled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 529549)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 50.waves)

        val anotherBobOrderId = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)
        dex1.api.place(anotherBobOrderId)

        waitForOrderAtNode(anotherBobOrderId)
        waitForOrderAtNode(aliceOrderId)

        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 1059099L)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 100.waves)
      }

      withClue("price asset is not fee asset") {

        dex1.safeRestartWithNewSuiteConfig(
          ConfigFactory.parseString(
            s""" waves.dex.order-fee.-1.composite.discount.asset="$UsdId" """
          ).withFallback(dexInitialSuiteConfig)
        )

        val aliceUsdBalance = wavesNode1.api.balance(alice, usd)
        val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
        val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)

        val aliceOrderId = mkOrder(alice, wavesBtcPair, OrderType.SELL, 100.waves, 10591, 1, feeAsset = usd)
        val bobOrderId = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)

        dex1.api.place(aliceOrderId)
        dex1.api.place(bobOrderId)

        dex1.api.waitForOrderStatus(aliceOrderId, Status.PartiallyFilled)
        dex1.api.waitForOrderStatus(bobOrderId, Status.Filled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1.api.balance(alice, usd) shouldBe (aliceUsdBalance - 1)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 529550)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 50.waves)

        val anotherBobOrderId = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)
        dex1.api.place(anotherBobOrderId)

        waitForOrderAtNode(anotherBobOrderId)
        waitForOrderAtNode(aliceOrderId)

        wavesNode1.api.balance(alice, usd) shouldBe (aliceUsdBalance - 1)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 1059100L)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 100.waves)
      }

      Seq(btc, usd).foreach(dex1.api.deleteAssetRate)
    }

    "percent & fixed fee modes" in {
      val ethRate = 0.000032

      upsertAssetRate(eth -> ethRate)

      def check(): Unit = {
        withClue("buy order") {

          val aliceBalance = wavesNode1.api.balance(alice, Waves)
          val bobBalance = wavesNode1.api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1.api.balance(alice, eth)
          val bobEthBalance = wavesNode1.api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 100, 100000000L, 10, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.getReservedBalanceWithApiKey(alice)(Waves) shouldBe 100

          dex1.api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, feeAsset = eth))
          waitForOrderAtNode(aliceOrderId)

          wavesNode1.api.balance(alice, Waves) shouldBe (aliceBalance - 100)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobBalance + 100)

          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance + 90)
          wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 110)

          eventually {
            dex1.api.getReservedBalanceWithApiKey(alice) shouldBe empty
          }

          eventually {
            dex1.api.getReservedBalanceWithApiKey(bob) shouldBe empty
          }
        }

        withClue("place buy order with amount less than fee") {

          val aliceBalance = wavesNode1.api.balance(alice, Waves)
          val bobBalance = wavesNode1.api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1.api.balance(alice, eth)
          val bobEthBalance = wavesNode1.api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 3, 100000000L, 10, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(eth -> 10, Waves -> 3)

          dex1.api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 3, 100000000L, 10, feeAsset = eth))

          waitForOrderAtNode(aliceOrderId)

          wavesNode1.api.balance(alice, Waves) shouldBe (aliceBalance - 3)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobBalance + 3)

          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 7)
          wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 13)

          dex1.api.getReservedBalanceWithApiKey(alice) shouldBe empty
          dex1.api.getReservedBalanceWithApiKey(bob) shouldBe empty
        }

        withClue("place buy order after partial fill") {

          val aliceBalance = wavesNode1.api.balance(alice, Waves)
          val bobBalance = wavesNode1.api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1.api.balance(alice, eth)
          val bobEthBalance = wavesNode1.api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 200, 100000000L, 20, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(eth -> 20, Waves -> 200)

          dex1.api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, feeAsset = eth))
          waitForOrderAtNode(aliceOrderId)

          wavesNode1.api.balance(alice, Waves) shouldBe (aliceBalance - 100)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobBalance + 100)

          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance + 90)
          wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 110)

          dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(eth -> 10, Waves -> 100)
          dex1.api.getReservedBalanceWithApiKey(bob) shouldBe empty

          dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, aliceOrderId)
        }

        withClue("place sell order") {
          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(eth -> 110)
          dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, aliceOrderId)
        }
      }

      broadcastAndAwait(mkTransfer(alice, bob, defaultAssetQuantity / 2, eth, 0.005.waves))

      dex1.safeRestartWithNewSuiteConfig(
        ConfigFactory.parseString("waves.dex.order-fee.-1.composite.default.mode = percent").withFallback(dexInitialSuiteConfig)
      )
      check()

      dex1.safeRestartWithNewSuiteConfig(
        ConfigFactory.parseString("waves.dex.order-fee.-1.composite.default.mode = fixed").withFallback(dexInitialSuiteConfig)
      )
      check()

      dex1.safeRestartWithNewSuiteConfig(
        ConfigFactory
          .parseString(
            s"""
               |waves.dex.order-fee.-1.composite.default {
               |  mode = fixed
               |  fixed.asset = $BtcId
               |  discount.asset = $BtcId
               |}
               |""".stripMargin
          )
          .withFallback(dexInitialSuiteConfig)
      )

      withClue("fee asset isn't part of asset pair") {
        broadcastAndAwait(mkTransfer(bob, alice, 100000000, btc))
        val orderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 200, 100000000L, 20, feeAsset = btc)
        dex1.api.place(orderId)

        dex1.api.getReservedBalanceWithApiKey(alice) shouldBe Map(Waves -> 200, btc -> 20)
        dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, orderId)
        dex1.api.getReservedBalanceWithApiKey(alice) shouldBe empty
      }
    }
  }
}
