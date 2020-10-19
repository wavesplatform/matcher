package com.wavesplatform.it.sync.orders

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.http.entities.HttpV0LevelAgg
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.{Order, OrderType}

// TODO refactor balances retrieving
class OrderDynamicFeeTestSuite extends OrderFeeBaseTestSuite {

  private val baseFee = 300000

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee.-1 {
       |    mode = dynamic
       |    dynamic {
       |      base-maker-fee = $baseFee
       |      base-taker-fee = $baseFee
       |    }
       |    percent {
       |      asset-type = amount
       |      min-fee = 10
       |    }
       |    fixed {
       |      asset = $EthId
       |      min-fee = 10
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueWctTx, IssueUsdTx, IssueEthTx, IssueBtcTx)
    dex1.start()
  }

  private def mkBobOrder: Order = mkOrder(
    owner = bob,
    pair = wavesBtcPair,
    orderType = OrderType.BUY,
    amount = 1.waves,
    price = 50000L,
    matcherFee = 150L,
    version = 3: Byte,
    feeAsset = btc
  )

  private def mkAliceOrder: Order = mkOrder(
    owner = alice,
    pair = wavesBtcPair,
    orderType = OrderType.SELL,
    amount = 1.waves,
    price = 50000L,
    matcherFee = 1920L,
    version = 3,
    feeAsset = eth
  )

  def upsertRates(pairs: (IssuedAsset, Double)*): Unit = pairs.foreach {
    case (asset, rate) => withClue(s"$asset")(dex1.api.upsertRate(asset, rate))
  }

  "supported non-waves order fee" - {
    val btcRate = 0.0005
    val ethRate = 0.0064

    "is not enough" in {
      upsertRates(btc -> btcRate, eth -> ethRate)
      dex1.tryApi.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 100L, // ^ 150
          feeAsset = btc
        )
      ) should failWith(
        9441542, // FeeNotEnough
        s"Required 0.0000015 $BtcId as fee for this order, but given 0.000001 $BtcId"
      ) // TODO

      // TODO
      val r = dex1.tryApi.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 1920L, // ^ 150
          feeAsset = eth // ^ BTC
        )
      )

      r should failWith(3147270, s"0.0000192 $EthId") // BalanceNotEnough
      r should failWith(3147270, s"0.0005 $BtcId") // BalanceNotEnough

      List(btc, eth).foreach(dex1.api.deleteRate)
    }

    "is enough" in {
      upsertRates(btc -> btcRate)
      dex1.api.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 150L,
          feeAsset = btc
        )
      )

      val before = dex1.api.reservedBalance(bob)
      before.keys should contain(btc)
      before(Waves) shouldEqual 100000000L
      dex1.api.cancelAll(bob)

      dex1.api.place(mkBobOrder)
      val after = dex1.api.reservedBalance(bob)
      after(btc) shouldEqual 50150L
      after.keys shouldNot contain(Waves)
      dex1.api.cancelAll(bob)
      dex1.api.deleteRate(btc)
    }

    "missing part of fee can be withdraw after order fill" in {
      upsertRates(eth -> ethRate)
      val bobEthBalance = wavesNode1.api.balance(bob, eth)
      if (bobEthBalance > 0) broadcastAndAwait(mkTransfer(bob, alice, bobEthBalance - 1920L, eth))
      else broadcastAndAwait(mkTransfer(alice, bob, 1920L, eth))
      val bobOrder = mkOrder(
        owner = bob,
        pair = ethWavesPair,
        orderType = OrderType.BUY,
        amount = 100000000L,
        price = 156250000000L,
        matcherFee = 1920L,
        feeAsset = eth
      )
      dex1.api.place(bobOrder)
      dex1.api.place(
        mkOrder(
          owner = alice,
          pair = ethWavesPair,
          orderType = OrderType.SELL,
          amount = 100000000L,
          price = 156250000000L,
          matcherFee = 1920L,
          feeAsset = eth
        )
      )
      waitForOrderAtNode(bobOrder)
      eventually {
        wavesNode1.api.balance(bob, eth) shouldBe 100000000L
      }
      dex1.api.deleteRate(eth)
    }
  }

  "asset fee is not supported" - {
    val btcRate = 0.0005
    val ethRate = 0.0064
    val order = mkBobOrder

    "only waves supported" in {
      dex1.tryApi.place(order) should failWith(
        9441540, // UnexpectedFeeAsset
        s"Required one of the following fee asset: WAVES. But given $BtcId"
      )
    }

    "not only waves supported" in {
      upsertRates(eth -> 0.1)
      dex1.tryApi.place(order) should failWith(
        9441540, // UnexpectedFeeAsset
        s"Required one of the following fee asset: $EthId, WAVES. But given $BtcId"
      )
      dex1.api.deleteRate(eth)
    }

    "asset became not supported after order was placed" in {
      upsertRates(btc -> btcRate, eth -> ethRate)

      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)

      dex1.api.place(order)
      dex1.api.deleteRate(btc)
      dex1.api.place(mkAliceOrder)
      dex1.api.waitForOrderStatus(order, Status.Filled)

      waitForOrderAtNode(order)

      eventually {
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 150L - 50000L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
      }

      dex1.api.deleteRate(eth)
    }

    "asset became not supported after order was partially filled" in {
      upsertRates(btc -> btcRate, eth -> ethRate)

      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)

      val aliceOrder = mkOrder(
        owner = alice,
        matcher = matcher,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 1920L,
        feeAsset = eth
      )

      dex1.api.place(aliceOrder)
      dex1.api.reservedBalance(alice)(eth) shouldBe 1920L
      dex1.api.place(mkBobOrder)
      dex1.api.waitForOrderStatus(aliceOrder, Status.PartiallyFilled)

      waitForOrderAtNode(aliceOrder)

      eventually {
        dex1.api.reservedBalance(alice)(eth) shouldBe 960L
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 150L - 50000L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
      }

      dex1.api.deleteRate(eth)

      val bobSecondOrder = mkBobOrder

      dex1.api.place(bobSecondOrder)
      dex1.api.waitForOrderStatus(aliceOrder, Status.Filled)

      waitForOrderAtNode(bobSecondOrder)

      eventually {
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 300L - 100000L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 100000L)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
      }

      dex1.api.deleteRate(btc)
    }

    "rates of asset pair was changed while order is placed" in {
      upsertRates(btc -> btcRate, eth -> ethRate)

      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val bobOrder = mkBobOrder

      dex1.api.place(bobOrder)

      val newBtcRate = btcRate * 2

      dex1.httpApi.upsertRate(btc, newBtcRate).code shouldBe StatusCodes.Ok
      dex1.api.reservedBalance(bob)(btc) shouldBe 50150L
      dex1.api.place(mkAliceOrder)

      waitForOrderAtNode(bobOrder)

      eventually(wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L))

      List(btc, eth).foreach(dex1.api.deleteRate)
    }
  }

  "orders with non-waves asset fee" - {
    val btcRate = 0.0005
    val ethRate = 0.0064

    "are full filled" in {
      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)
      val matcherEthBalance = wavesNode1.api.balance(matcher, eth)
      val matcherBtcBalance = wavesNode1.api.balance(matcher, btc)
      val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
      val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)

      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobOrder = mkBobOrder
      placeAndAwaitAtDex(bobOrder)
      dex1.api.reservedBalance(bob).keys should not contain Waves

      val aliceOrder = mkAliceOrder
      dex1.api.place(aliceOrder)

      List(bobOrder, aliceOrder).foreach(dex1.api.waitForOrderStatus(_, Status.Filled))
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

      eventually {
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
        wavesNode1.api.balance(matcher, eth) shouldBe (matcherEthBalance + 1920L)
        wavesNode1.api.balance(matcher, btc) shouldBe (matcherBtcBalance + 150L)
        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)
      }

      List(btc, eth).foreach(dex1.api.deleteRate)
    }

    "are partial filled" in {
      val bobBtcBalance = wavesNode1.api.balance(bob, btc)
      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceEthBalance = wavesNode1.api.balance(alice, eth)
      val matcherEthBalance = wavesNode1.api.balance(matcher, eth)

      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobOrder = mkBobOrder
      dex1.api.place(bobOrder)

      val aliceOrder = mkOrder(
        owner = alice,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 1920L,
        feeAsset = eth
      )
      dex1.api.place(aliceOrder)

      Map(bobOrder -> Status.Filled, aliceOrder -> Status.PartiallyFilled).foreach(Function.tupled(dex1.api.waitForOrderStatus))
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

      eventually {
        wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
        wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
        wavesNode1.api.balance(matcher, eth) shouldBe (matcherEthBalance + 960L)
      }

      dex1.api.cancelAll(alice)
      List(btc, eth).foreach(dex1.api.deleteRate)
    }

    "are partial filled both" in {
      val params = Map(9.waves -> 213L, 1900.waves -> 1L, 2000.waves -> 1L)
      for ((aliceOrderAmount, aliceBalanceDiff) <- params) {

        val bobBtcBalance = wavesNode1.api.balance(bob, btc)
        val bobWavesBalance = wavesNode1.api.balance(bob, Waves)

        val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
        val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
        val aliceEthBalance = wavesNode1.api.balance(alice, eth)

        val matcherEthBalance = wavesNode1.api.balance(matcher, eth)

        upsertRates(btc -> btcRate, eth -> ethRate)
        val bobOrder = mkBobOrder
        placeAndAwaitAtDex(bobOrder)
        dex1.api.reservedBalance(bob).keys should not contain Waves

        val aliceOrder = mkOrder(
          owner = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = aliceOrderAmount,
          price = 50000L,
          matcherFee = 1920L,
          feeAsset = eth
        )
        dex1.api.place(aliceOrder)

        Map(bobOrder -> Status.Filled, aliceOrder -> Status.PartiallyFilled).foreach(Function.tupled(dex1.api.waitForOrderStatus))
        List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

        eventually {
          wavesNode1.api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
          wavesNode1.api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - aliceBalanceDiff)
          wavesNode1.api.balance(matcher, eth) shouldBe (matcherEthBalance + aliceBalanceDiff)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
          wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)
        }

        dex1.api.cancelAll(alice)
        List(btc, eth).foreach(dex1.api.deleteRate)
      }
    }
  }

  "cancellation of" - {

    val btcRate = 0.0005
    val ethRate = 0.0064

    "order with non-waves fee" in {
      val bobBalance = dex1.api.tradableBalance(bob, wavesBtcPair)

      upsertRates(btc -> btcRate)

      val order = mkOrder(
        owner = bob,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 1.waves,
        price = 50000L,
        matcherFee = 150L,
        feeAsset = btc
      )

      dex1.api.place(order)
      dex1.api.cancel(bob, order).status shouldBe "OrderCanceled"
      dex1.api.reservedBalance(bob).keys.size shouldBe 0
      dex1.api.tradableBalance(bob, wavesBtcPair) shouldEqual bobBalance
      dex1.api.deleteRate(btc)
    }

    "partially filled order with non-waves fee" in {
      val aliceEthBalance = dex1.api.tradableBalance(alice, ethWavesPair)(eth)
      upsertRates(btc -> btcRate, eth -> ethRate)

      val bobOrder = mkOrder(
        owner = bob,
        pair = wavesBtcPair,
        orderType = OrderType.BUY,
        amount = 1.waves,
        price = 50000L,
        matcherFee = 150L,
        version = 3,
        feeAsset = btc
      )

      dex1.api.place(bobOrder)

      val aliceOrder = mkOrder(
        owner = alice,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 1920L,
        feeAsset = eth
      )

      dex1.api.place(aliceOrder)
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))
      dex1.api.cancel(alice, aliceOrder).status shouldBe "OrderCanceled"
      dex1.api.reservedBalance(alice).keys.size shouldBe 0
      wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
      List(btc, eth).foreach(dex1.api.deleteRate)
    }
  }

  "fee in pairs with different decimals count" in {
    upsertRates(usd -> 5d)
    dex1.tryApi.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, matcherFee = 1L, feeAsset = usd)) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.02 $UsdId as fee for this order, but given 0.01 $UsdId"
    )

    upsertRates(usd -> 3)
    broadcastAndAwait(mkTransfer(alice, bob.toAddress, 1L, usd))

    val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
    val aliceUsdBalance = wavesNode1.api.balance(alice, usd)
    val bobWavesBalance = wavesNode1.api.balance(bob, Waves)
    val bobUsdBalance = wavesNode1.api.balance(bob, usd)

    val bobOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, matcherFee = 1L, feeAsset = usd)
    dex1.api.place(bobOrder)

    dex1.api.orderBook(wavesUsdPair).asks shouldBe List(HttpV0LevelAgg(1.waves, 300))
    dex1.api.reservedBalance(bob) shouldBe Map(usd -> 1L, Waves -> 1.waves)
    dex1.api.cancel(bob, bobOrder)

    wavesNode1.api.balance(alice, Waves) shouldBe aliceWavesBalance
    wavesNode1.api.balance(alice, usd) shouldBe aliceUsdBalance

    wavesNode1.api.balance(bob, Waves) shouldBe bobWavesBalance
    wavesNode1.api.balance(bob, usd) shouldBe bobUsdBalance

    val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300, matcherFee = 1L, feeAsset = usd)
    dex1.api.place(aliceOrderId)

    dex1.api.orderBook(wavesUsdPair).bids shouldBe List(HttpV0LevelAgg(1.waves, 300))
    dex1.api.reservedBalance(alice) shouldBe Map(usd -> 301)

    dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, 1L, feeAsset = usd))

    waitForOrderAtNode(aliceOrderId)
    wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance + 1.waves)
    wavesNode1.api.balance(alice, usd) shouldBe (aliceUsdBalance - 301)

    wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance - 1.waves)
    wavesNode1.api.balance(bob, usd) shouldBe (bobUsdBalance + 299)

    dex1.api.deleteRate(usd)
  }

  "rounding fee to filled amount" - {
    "if amount cannot be filled" in {

      Seq(wct, btc, usd).foreach(asset => upsertRates(asset -> 0.000003d))

      withClue("price asset is fee asset") {

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

        dex1.api.cancel(alice, aliceOrderId)
      }

      withClue("price asset is not fee asset") {

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

        dex1.api.cancel(alice, aliceOrderId)
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

        dex1.api.cancel(bob, bobOrderId)
      }

      Seq(wct, btc, usd).foreach(dex1.api.deleteRate)
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

      dex1.api.cancelAll(alice)
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

      dex1.api.cancelAll(alice)
    }

    "if order was filled partially" in {

      Seq(btc, usd).foreach(asset => upsertRates(asset -> 0.000003d))

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

      Seq(btc, usd).foreach(dex1.api.deleteRate)
    }

    "percent & fixed fee modes" in {

      def check(): Unit = {
        withClue("buy order") {

          val aliceBalance = wavesNode1.api.balance(alice, Waves)
          val bobBalance = wavesNode1.api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1.api.balance(alice, eth)
          val bobEthBalance = wavesNode1.api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 100, 100000000L, 10, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.reservedBalance(alice)(Waves) shouldBe 100

          dex1.api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, feeAsset = eth))
          waitForOrderAtNode(aliceOrderId)

          wavesNode1.api.balance(alice, Waves) shouldBe (aliceBalance - 100)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobBalance + 100)

          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance + 90)
          wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 110)

          dex1.api.reservedBalance(alice) shouldBe empty
          dex1.api.reservedBalance(bob) shouldBe empty
        }

        withClue("place buy order with amount less than fee") {

          val aliceBalance = wavesNode1.api.balance(alice, Waves)
          val bobBalance = wavesNode1.api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1.api.balance(alice, eth)
          val bobEthBalance = wavesNode1.api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 3, 100000000L, 10, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.reservedBalance(alice) shouldBe Map(eth -> 10, Waves -> 3)

          dex1.api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 3, 100000000L, 10, feeAsset = eth))

          waitForOrderAtNode(aliceOrderId)

          wavesNode1.api.balance(alice, Waves) shouldBe (aliceBalance - 3)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobBalance + 3)

          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance - 7)
          wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 13)

          dex1.api.reservedBalance(alice) shouldBe empty
          dex1.api.reservedBalance(bob) shouldBe empty
        }

        withClue("place buy order after partial fill") {

          val aliceBalance = wavesNode1.api.balance(alice, Waves)
          val bobBalance = wavesNode1.api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1.api.balance(alice, eth)
          val bobEthBalance = wavesNode1.api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 200, 100000000L, 20, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.reservedBalance(alice) shouldBe Map(eth -> 20, Waves -> 200)

          dex1.api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, feeAsset = eth))
          waitForOrderAtNode(aliceOrderId)

          wavesNode1.api.balance(alice, Waves) shouldBe (aliceBalance - 100)
          wavesNode1.api.balance(bob, Waves) shouldBe (bobBalance + 100)

          wavesNode1.api.balance(alice, eth) shouldBe (aliceEthBalance + 90)
          wavesNode1.api.balance(bob, eth) shouldBe (bobEthBalance - 110)

          dex1.api.reservedBalance(alice) shouldBe Map(eth -> 10, Waves -> 100)
          dex1.api.reservedBalance(bob) shouldBe empty

          dex1.api.cancel(alice, aliceOrderId)
        }

        withClue("place sell order") {
          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, feeAsset = eth)
          dex1.api.place(aliceOrderId)

          dex1.api.reservedBalance(alice) shouldBe Map(eth -> 110)
          dex1.api.cancel(alice, aliceOrderId)
        }
      }

      broadcastAndAwait(mkTransfer(alice, bob, defaultAssetQuantity / 2, eth, 0.005.waves))

      dex1.restartWithNewSuiteConfig(ConfigFactory.parseString("waves.dex.order-fee.-1.mode = percent"))
      check()

      dex1.restartWithNewSuiteConfig(ConfigFactory.parseString("waves.dex.order-fee.-1.mode = fixed").withFallback(dexInitialSuiteConfig))
      check()

      dex1.restartWithNewSuiteConfig(
        ConfigFactory
          .parseString(s"waves.dex.order-fee.-1.fixed.asset = $BtcId\nwaves.dex.order-fee.-1.mode = fixed")
          .withFallback(dexInitialSuiteConfig)
      )

      withClue("fee asset isn't part of asset pair") {
        broadcastAndAwait(mkTransfer(bob, alice, 100000000, btc))
        val orderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 200, 100000000L, 20, feeAsset = btc)
        dex1.api.place(orderId)

        dex1.api.reservedBalance(alice) shouldBe Map(Waves -> 200, btc -> 20)
        dex1.api.cancel(alice, orderId)
        dex1.api.reservedBalance(alice) shouldBe empty
      }
    }
  }
}
