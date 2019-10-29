package com.wavesplatform.it.sync

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

// TODO refactor balances retrieving
class OrderFeeTestSuite extends MatcherSuiteBase {

  private val baseFee = 300000

  override protected def suiteInitialDexConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee {
       |    mode = dynamic
       |    dynamic {
       |      base-fee = $baseFee
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
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueWctTx, IssueUsdTx, IssueEthTx, IssueBtcTx)
    startAndWait(dex1Container(), dex1Api)
  }

  private def mkBobOrder: Order = mkOrder(
    owner = bob,
    pair = wavesBtcPair,
    orderType = OrderType.BUY,
    amount = 1.waves,
    price = 50000L,
    matcherFee = 150L,
    version = 3: Byte,
    matcherFeeAssetId = btc
  )

  private def mkAliceOrder: Order = mkOrder(
    owner = alice,
    pair = wavesBtcPair,
    orderType = OrderType.SELL,
    amount = 1.waves,
    price = 50000L,
    matcherFee = 1920L,
    version = 3,
    matcherFeeAssetId = eth
  )

  def upsertRates(pairs: (IssuedAsset, Double)*): Unit = pairs.foreach {
    case (asset, rate) => withClue(s"$asset")(dex1Api.upsertRate(asset, rate))
  }

  "supported non-waves order fee" - {
    val btcRate = 0.0005
    val ethRate = 0.0064

    "is not enough" in {
      upsertRates(btc -> btcRate, eth -> ethRate)
      dex1Api.tryPlace(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 100L, // ^ 150
          matcherFeeAssetId = btc
        )
      ) should failWith(
        9441542, // FeeNotEnough
        s"Required 0.0000015 $BtcId as fee for this order, but given 0.000001 $BtcId"
      ) // TODO

      // TODO
      val r = dex1Api.tryPlace(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 1920L, // ^ 150
          matcherFeeAssetId = eth // ^ BTC
        )
      )

      r should failWith(3147270, s"0.0000192 $EthId") // BalanceNotEnough
      r should failWith(3147270, s"0.0005 $BtcId")    // BalanceNotEnough

      List(btc, eth).foreach(dex1Api.deleteRate)
    }

    "is enough" in {

      upsertRates(btc -> btcRate)
      dex1Api.place(
        mkOrder(
          owner = bob,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          matcherFee = 150L,
          matcherFeeAssetId = btc
        )
      )

      val before = dex1Api.reservedBalance(bob)
      before.keys shouldNot contain(btc)
      before(Waves) shouldEqual 100000000L
      dex1Api.cancelAll(bob)

      dex1Api.place(mkBobOrder)
      val after = dex1Api.reservedBalance(bob)
      after(btc) shouldEqual 50150L
      after.keys shouldNot contain(Waves)
      dex1Api.cancelAll(bob)
      dex1Api.deleteRate(btc)
    }

    "missing part of fee can be withdraw after order fill" in {
      upsertRates(eth -> ethRate)
      val bobEthBalance = wavesNode1Api.balance(bob, eth)
      if (bobEthBalance > 0) wavesNode1Api.broadcast(mkTransfer(bob, alice, bobEthBalance, eth))
      val bobOrder = mkOrder(
        owner = bob,
        pair = ethWavesPair,
        orderType = OrderType.BUY,
        amount = 100000000L,
        price = 156250000000L,
        matcherFee = 1920L,
        matcherFeeAssetId = eth
      )
      dex1Api.place(bobOrder)
      dex1Api.place(
        mkOrder(
          owner = alice,
          pair = ethWavesPair,
          orderType = OrderType.SELL,
          amount = 100000000L,
          price = 156250000000L,
          matcherFee = 1920L,
          matcherFeeAssetId = eth
        ))
      waitForOrderAtNode(bobOrder)
      eventually {
        wavesNode1Api.balance(bob, eth) shouldBe (100000000L - 1920L)
      }
      dex1Api.deleteRate(eth)
    }
  }

  "asset fee is not supported" - {
    val btcRate = 0.0005
    val ethRate = 0.0064
    val order   = mkBobOrder

    "only waves supported" in {
      dex1Api.tryPlace(order) should failWith(
        9441540, // UnexpectedFeeAsset
        s"Required one of the following fee asset: WAVES. But given $BtcId"
      )
    }

    "not only waves supported" in {
      upsertRates(eth -> 0.1)
      dex1Api.tryPlace(order) should failWith(
        9441540, // UnexpectedFeeAsset
        s"Required one of the following fee asset: $EthId, WAVES. But given $BtcId"
      )
      dex1Api.deleteRate(eth)
    }

    "asset became not supported after order was placed" in {
      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobBtcBalance   = wavesNode1Api.balance(bob, btc)
      val aliceBtcBalance = wavesNode1Api.balance(alice, btc)
      val aliceEthBalance = wavesNode1Api.balance(alice, eth)
      dex1Api.place(order)
      dex1Api.deleteRate(btc)
      dex1Api.place(mkAliceOrder)
      dex1Api.waitForOrderStatus(order, OrderStatus.Filled)
      waitForOrderAtNode(order)
      eventually {
        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 150L - 50000L)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
      }
      dex1Api.deleteRate(eth)
    }

    "asset became not supported after order was partially filled" in {
      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobBtcBalance   = wavesNode1Api.balance(bob, btc)
      val aliceBtcBalance = wavesNode1Api.balance(alice, btc)
      val aliceEthBalance = wavesNode1Api.balance(alice, eth)
      val aliceOrder = mkOrder(
        owner = alice,
        matcher = matcher,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 1920L,
        matcherFeeAssetId = eth
      )
      dex1Api.place(aliceOrder)
      dex1Api.reservedBalance(alice)(eth) shouldBe 1920L
      dex1Api.place(mkBobOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.PartiallyFilled)
      waitForOrderAtNode(aliceOrder)
      eventually {
        dex1Api.reservedBalance(alice)(eth) shouldBe 960L
        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 150L - 50000L)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
      }

      dex1Api.deleteRate(eth)

      val bobSecondOrder = mkBobOrder
      dex1Api.place(bobSecondOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)
      waitForOrderAtNode(bobSecondOrder)
      eventually {
        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 300L - 100000L)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 100000L)
        wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
      }
      dex1Api.deleteRate(btc)
    }

    "rates of asset pair was changed while order is placed" in {
      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobBtcBalance = wavesNode1Api.balance(bob, btc)
      val bobOrder      = mkBobOrder
      dex1Api.place(bobOrder)

      val newBtcRate = btcRate * 2
      dex1Api.upsertRate(btc, newBtcRate)._1 shouldBe StatusCodes.Ok
      dex1Api.reservedBalance(bob)(btc) shouldBe 50150L
      dex1Api.place(mkAliceOrder)
      waitForOrderAtNode(bobOrder)
      eventually {
        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
      }
      List(btc, eth).foreach(dex1Api.deleteRate)
    }
  }

  "orders with non-waves asset fee" - {
    val btcRate = 0.0005
    val ethRate = 0.0064

    "are full filled" in {
      val bobBtcBalance     = wavesNode1Api.balance(bob, btc)
      val aliceBtcBalance   = wavesNode1Api.balance(alice, btc)
      val aliceEthBalance   = wavesNode1Api.balance(alice, eth)
      val matcherEthBalance = wavesNode1Api.balance(matcher, eth)
      val matcherBtcBalance = wavesNode1Api.balance(matcher, btc)
      val bobWavesBalance   = wavesNode1Api.balance(bob, Waves)
      val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)

      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobOrder = mkBobOrder
      placeAndAwait(bobOrder)
      dex1Api.reservedBalance(bob).keys should not contain Waves

      val aliceOrder = mkAliceOrder
      dex1Api.place(aliceOrder)

      List(bobOrder, aliceOrder).foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Filled))
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

      eventually {
        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
        wavesNode1Api.balance(matcher, eth) shouldBe (matcherEthBalance + 1920L)
        wavesNode1Api.balance(matcher, btc) shouldBe (matcherBtcBalance + 150L)
        wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)
      }
      List(btc, eth).foreach(dex1Api.deleteRate)
    }

    "are partial filled" in {
      val bobBtcBalance     = wavesNode1Api.balance(bob, btc)
      val aliceBtcBalance   = wavesNode1Api.balance(alice, btc)
      val aliceEthBalance   = wavesNode1Api.balance(alice, eth)
      val matcherEthBalance = wavesNode1Api.balance(matcher, eth)

      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobOrder = mkBobOrder
      dex1Api.place(bobOrder)

      val aliceOrder = mkOrder(
        owner = alice,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 1920L,
        matcherFeeAssetId = eth
      )
      dex1Api.place(aliceOrder)

      Map(bobOrder -> OrderStatus.Filled, aliceOrder -> OrderStatus.PartiallyFilled).foreach(Function.tupled(dex1Api.waitForOrderStatus))
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

      eventually {
        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
        wavesNode1Api.balance(matcher, eth) shouldBe (matcherEthBalance + 960L)
      }

      dex1Api.cancelAll(alice)
      List(btc, eth).foreach(dex1Api.deleteRate)
    }

    "are partial filled both" in {
      val params = Map(9.waves -> 213L, 1900.waves -> 1L, 2000.waves -> 1L)
      for ((aliceOrderAmount, aliceBalanceDiff) <- params) {

        val bobBtcBalance   = wavesNode1Api.balance(bob, btc)
        val bobWavesBalance = wavesNode1Api.balance(bob, Waves)

        val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)
        val aliceBtcBalance   = wavesNode1Api.balance(alice, btc)
        val aliceEthBalance   = wavesNode1Api.balance(alice, eth)

        val matcherEthBalance = wavesNode1Api.balance(matcher, eth)

        upsertRates(btc -> btcRate, eth -> ethRate)
        val bobOrder = mkBobOrder
        placeAndAwait(bobOrder)
        dex1Api.reservedBalance(bob).keys should not contain Waves

        val aliceOrder = mkOrder(
          owner = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = aliceOrderAmount,
          price = 50000L,
          matcherFee = 1920L,
          matcherFeeAssetId = eth
        )
        dex1Api.place(aliceOrder)

        Map(bobOrder -> OrderStatus.Filled, aliceOrder -> OrderStatus.PartiallyFilled).foreach(Function.tupled(dex1Api.waitForOrderStatus))
        List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))

        eventually {
          wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
          wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
          wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - aliceBalanceDiff)
          wavesNode1Api.balance(matcher, eth) shouldBe (matcherEthBalance + aliceBalanceDiff)
          wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
          wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)
        }

        dex1Api.cancelAll(alice)
        List(btc, eth).foreach(dex1Api.deleteRate)
      }
    }
  }

  "cancellation of" - {
    val btcRate = 0.0005
    val ethRate = 0.0064
    "order with non-waves fee" in {
      val bobBalance = dex1Api.tradableBalance(bob, wavesBtcPair)
      upsertRates(btc -> btcRate)
      val order = mkOrder(
        owner = bob,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 1.waves,
        price = 50000L,
        matcherFee = 150L,
        matcherFeeAssetId = btc
      )
      dex1Api.place(order)
      dex1Api.cancel(bob, order).status shouldBe "OrderCanceled"
      dex1Api.reservedBalance(bob).keys.size shouldBe 0
      dex1Api.tradableBalance(bob, wavesBtcPair) shouldEqual bobBalance
      dex1Api.deleteRate(btc)
    }

    "partially filled order with non-waves fee" in {
      val aliceEthBalance = dex1Api.tradableBalance(alice, ethWavesPair)(eth)
      upsertRates(btc -> btcRate, eth -> ethRate)
      val bobOrder = mkOrder(
        owner = bob,
        pair = wavesBtcPair,
        orderType = OrderType.BUY,
        amount = 1.waves,
        price = 50000L,
        matcherFee = 150L,
        version = 3,
        matcherFeeAssetId = btc
      )
      dex1Api.place(bobOrder)
      val aliceOrder = mkOrder(
        owner = alice,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 2.waves,
        price = 50000L,
        matcherFee = 1920L,
        matcherFeeAssetId = eth
      )
      dex1Api.place(aliceOrder)
      List(bobOrder, aliceOrder).foreach(waitForOrderAtNode(_))
      dex1Api.cancel(alice, aliceOrder).status shouldBe "OrderCanceled"
      dex1Api.reservedBalance(alice).keys.size shouldBe 0
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
      List(btc, eth).foreach(dex1Api.deleteRate)
    }
  }

  "fee in pairs with different decimals count" in {
    upsertRates(usd -> 5d)
    dex1Api.tryPlace(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, matcherFee = 1L, matcherFeeAssetId = usd)) should failWith(
      9441542, // FeeNotEnough
      s"Required 0.02 $UsdId as fee for this order, but given 0.01 $UsdId"
    )

    upsertRates(usd -> 3)

    val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)
    val aliceUsdBalance   = wavesNode1Api.balance(alice, usd)
    val bobWavesBalance   = wavesNode1Api.balance(bob, Waves)
    val bobUsdBalance     = wavesNode1Api.balance(bob, usd)

    val bobOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, matcherFee = 1L, matcherFeeAssetId = usd)
    dex1Api.place(bobOrder)

    dex1Api.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1.waves, 300))
    dex1Api.reservedBalance(bob) shouldBe Map(Waves -> 1.waves)
    dex1Api.cancel(bob, bobOrder)

    wavesNode1Api.balance(alice, Waves) shouldBe aliceWavesBalance
    wavesNode1Api.balance(alice, usd) shouldBe aliceUsdBalance

    wavesNode1Api.balance(bob, Waves) shouldBe bobWavesBalance
    wavesNode1Api.balance(bob, usd) shouldBe bobUsdBalance

    val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300, matcherFee = 1L, matcherFeeAssetId = usd)
    dex1Api.place(aliceOrderId)

    dex1Api.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1.waves, 300))
    dex1Api.reservedBalance(alice) shouldBe Map(usd -> 301)

    dex1Api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 300, 1L, matcherFeeAssetId = usd))

    waitForOrderAtNode(aliceOrderId)
    wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance + 1.waves)
    wavesNode1Api.balance(alice, usd) shouldBe (aliceUsdBalance - 301)

    wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance - 1.waves)
    wavesNode1Api.balance(bob, usd) shouldBe (bobUsdBalance + 299)

    dex1Api.deleteRate(usd)
  }

  "rounding fee to filled amount" - {
    "if amount cannot be filled" in {

      Seq(wct, btc, usd).foreach(asset => upsertRates(asset -> 0.000003D))

      withClue("price asset is fee asset") {

        val bobWctBalance   = wavesNode1Api.balance(bob, wct)
        val bobWavesBalance = wavesNode1Api.balance(bob, Waves)
        val bobUsdBalance   = wavesNode1Api.balance(bob, usd)

        val bobOrderId   = mkOrder(bob, wavesUsdPair, OrderType.SELL, 425532L, 238, 1, matcherFeeAssetId = wct)
        val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 238, matcherFee, version = 3)

        dex1Api.place(bobOrderId)
        dex1Api.place(aliceOrderId)

        dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Filled)
        dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.PartiallyFilled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance - 420169L)
        wavesNode1Api.balance(bob, usd) shouldBe (bobUsdBalance + 1)
        wavesNode1Api.balance(bob, wct) shouldBe (bobWctBalance - 1)

        dex1Api.cancel(alice, aliceOrderId)
      }

      withClue("price asset is not fee asset") {

        val bobWavesBalance = wavesNode1Api.balance(bob, Waves)
        val bobUsdBalance   = wavesNode1Api.balance(bob, usd)

        val bobOrderId   = mkOrder(bob, wavesUsdPair, OrderType.SELL, 851064L, 238, 1, matcherFeeAssetId = usd)
        val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 238, matcherFee, version = 3)

        dex1Api.place(bobOrderId)
        dex1Api.place(aliceOrderId)

        dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Filled)
        dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.PartiallyFilled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance - 840337L)
        wavesNode1Api.balance(bob, usd) shouldBe (bobUsdBalance + 1)

        dex1Api.cancel(alice, aliceOrderId)
      }

      withClue("buy order") {

        broadcastAndAwait { mkTransfer(bob, alice, 1, wct, 0.001.waves) }

        val aliceWctBalance   = wavesNode1Api.balance(alice, wct)
        val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)
        val aliceUsdBalance   = wavesNode1Api.balance(alice, usd)

        val aliceOrderId = mkOrder(alice, wavesUsdPair, OrderType.BUY, 851064L, 238, 1, matcherFeeAssetId = wct)
        val bobOrderId   = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 238, matcherFee, version = 3)

        dex1Api.place(aliceOrderId)
        dex1Api.place(bobOrderId)

        dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.Filled)
        dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.PartiallyFilled)

        waitForOrderAtNode(aliceOrderId)

        wavesNode1Api.balance(alice, wct) shouldBe (aliceWctBalance - 1)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance + 840337L)
        wavesNode1Api.balance(alice, usd) shouldBe (aliceUsdBalance - 2)

        dex1Api.cancel(bob, bobOrderId)
      }

      Seq(wct, btc, usd).foreach(dex1Api.deleteRate)
    }

    "if order was filled partially" in {

      Seq(btc, usd).foreach(asset => upsertRates(asset -> 0.000003D))

      withClue("price asset is fee asset") {

        val aliceBtcBalance   = wavesNode1Api.balance(alice, btc)
        val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)

        val aliceOrderId = mkOrder(alice, wavesBtcPair, OrderType.SELL, 100.waves, 10591, 1, matcherFeeAssetId = btc)
        val bobOrderId   = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)

        dex1Api.place(aliceOrderId)
        dex1Api.place(bobOrderId)

        dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.PartiallyFilled)
        dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Filled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 529549)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 50.waves)

        val anotherBobOrderId = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)
        dex1Api.place(anotherBobOrderId)

        waitForOrderAtNode(anotherBobOrderId)
        waitForOrderAtNode(aliceOrderId)

        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 1059099L)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 100.waves)
      }

      withClue("price asset is not fee asset") {

        val aliceUsdBalance   = wavesNode1Api.balance(alice, usd)
        val aliceBtcBalance   = wavesNode1Api.balance(alice, btc)
        val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)

        val aliceOrderId = mkOrder(alice, wavesBtcPair, OrderType.SELL, 100.waves, 10591, 1, matcherFeeAssetId = usd)
        val bobOrderId   = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)

        dex1Api.place(aliceOrderId)
        dex1Api.place(bobOrderId)

        dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.PartiallyFilled)
        dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Filled)

        waitForOrderAtNode(bobOrderId)

        wavesNode1Api.balance(alice, usd) shouldBe (aliceUsdBalance - 1)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 529550)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 50.waves)

        val anotherBobOrderId = mkOrder(bob, wavesBtcPair, OrderType.BUY, 50.waves, 10591, matcherFee, version = 3)
        dex1Api.place(anotherBobOrderId)

        waitForOrderAtNode(anotherBobOrderId)
        waitForOrderAtNode(aliceOrderId)

        wavesNode1Api.balance(alice, usd) shouldBe (aliceUsdBalance - 1)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 1059100L)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 100.waves)
      }

      Seq(btc, usd).foreach(dex1Api.deleteRate)
    }

    "percent & fixed fee modes" in {

      def check(): Unit = {
        withClue("buy order") {

          val aliceBalance    = wavesNode1Api.balance(alice, Waves)
          val bobBalance      = wavesNode1Api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1Api.balance(alice, eth)
          val bobEthBalance   = wavesNode1Api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 100, 100000000L, 10, matcherFeeAssetId = eth)
          dex1Api.place(aliceOrderId)

          dex1Api.reservedBalance(alice) shouldBe Map(Waves -> 100)

          dex1Api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, matcherFeeAssetId = eth))
          waitForOrderAtNode(aliceOrderId)

          wavesNode1Api.balance(alice, Waves) shouldBe (aliceBalance - 100)
          wavesNode1Api.balance(bob, Waves) shouldBe (bobBalance + 100)

          wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance + 90)
          wavesNode1Api.balance(bob, eth) shouldBe (bobEthBalance - 110)

          dex1Api.reservedBalance(alice) shouldBe empty
          dex1Api.reservedBalance(bob) shouldBe empty
        }

        withClue("place buy order with amount less than fee") {

          val aliceBalance    = wavesNode1Api.balance(alice, Waves)
          val bobBalance      = wavesNode1Api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1Api.balance(alice, eth)
          val bobEthBalance   = wavesNode1Api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 3, 100000000L, 10, matcherFeeAssetId = eth)
          dex1Api.place(aliceOrderId)

          dex1Api.reservedBalance(alice) shouldBe Map(eth -> 7, Waves -> 3)

          dex1Api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 3, 100000000L, 10, matcherFeeAssetId = eth))

          waitForOrderAtNode(aliceOrderId)

          wavesNode1Api.balance(alice, Waves) shouldBe (aliceBalance - 3)
          wavesNode1Api.balance(bob, Waves) shouldBe (bobBalance + 3)

          wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 7)
          wavesNode1Api.balance(bob, eth) shouldBe (bobEthBalance - 13)

          dex1Api.reservedBalance(alice) shouldBe empty
          dex1Api.reservedBalance(bob) shouldBe empty
        }

        withClue("place buy order after partial fill") {

          val aliceBalance    = wavesNode1Api.balance(alice, Waves)
          val bobBalance      = wavesNode1Api.balance(bob, Waves)
          val aliceEthBalance = wavesNode1Api.balance(alice, eth)
          val bobEthBalance   = wavesNode1Api.balance(bob, eth)

          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 200, 100000000L, 20, matcherFeeAssetId = eth)
          dex1Api.place(aliceOrderId)

          dex1Api.reservedBalance(alice) shouldBe Map(Waves -> 200)

          dex1Api.place(mkOrder(bob, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, matcherFeeAssetId = eth))
          waitForOrderAtNode(aliceOrderId)

          wavesNode1Api.balance(alice, Waves) shouldBe (aliceBalance - 100)
          wavesNode1Api.balance(bob, Waves) shouldBe (bobBalance + 100)

          wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance + 90)
          wavesNode1Api.balance(bob, eth) shouldBe (bobEthBalance - 110)

          dex1Api.reservedBalance(alice) shouldBe Map(Waves -> 100)
          dex1Api.reservedBalance(bob) shouldBe empty

          dex1Api.cancel(alice, aliceOrderId)
        }

        withClue("place sell order") {
          val aliceOrderId = mkOrder(alice, ethWavesPair, OrderType.SELL, 100, 100000000L, 10, matcherFeeAssetId = eth)
          dex1Api.place(aliceOrderId)

          dex1Api.reservedBalance(alice) shouldBe Map(eth -> 110)
          dex1Api.cancel(alice, aliceOrderId)
        }
      }

      broadcastAndAwait { mkTransfer(alice, bob, defaultAssetQuantity / 2, eth, 0.005.waves) }

      replaceSuiteConfig(dex1Container(), ConfigFactory.parseString("waves.dex.order-fee.mode = percent"))
      restartContainer(dex1Container(), dex1Api)
      check()

      replaceSuiteConfig(
        dex1Container(),
        ConfigFactory.parseString("waves.dex.order-fee.mode = fixed").withFallback(suiteInitialDexConfig)
      )

      restartContainer(dex1Container(), dex1Api)
      check()

      replaceSuiteConfig(
        dex1Container(),
        ConfigFactory.parseString(s"waves.dex.order-fee.fixed.asset = $BtcId\nwaves.dex.order-fee.mode = fixed").withFallback(suiteInitialDexConfig)
      )

      restartContainer(dex1Container(), dex1Api)

      withClue("fee asset isn't part of asset pair") {
        broadcastAndAwait(mkTransfer(bob, alice, 100000000, btc))
        val orderId = mkOrder(alice, ethWavesPair, OrderType.BUY, 200, 100000000L, 20, matcherFeeAssetId = btc)
        dex1Api.place(orderId)

        dex1Api.reservedBalance(alice) shouldBe Map(Waves -> 200, btc -> 20)
        dex1Api.cancel(alice, orderId)
        dex1Api.reservedBalance(alice) shouldBe empty
      }
    }
  }
}
