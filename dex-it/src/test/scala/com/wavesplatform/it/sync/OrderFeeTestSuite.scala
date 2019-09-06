package com.wavesplatform.it.sync

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

// TODO refactor balances retrieving
class OrderFeeTestSuite extends NewMatcherSuiteBase {
  private val baseFee = 300000
  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.order-fee {
       |  mode = dynamic
       |  dynamic.base-fee = $baseFee
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueUsdTx, IssueEthTx, IssueBtcTx)
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
        )) should failWith(9441542, s"Required 0.0000015 $BtcId as fee for this order, but given 0.000001 $BtcId") // TODO

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
        ))
      r should failWith(3147270, s"0.0000192 $EthId")
      r should failWith(3147270, s"0.0005 $BtcId")

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
      waitForOrderAtNode(bobOrder.id())
      wavesNode1Api.balance(bob, eth) shouldBe (100000000L - 1920L)
      dex1Api.deleteRate(eth)
    }
  }

  "asset fee is not supported" - {
    val btcRate = 0.0005
    val ethRate = 0.0064
    val order   = mkBobOrder

    "only waves supported" in {
      dex1Api.tryPlace(order) should failWith(9441540, s"Required one of the following fee asset: WAVES. But given $BtcId")
    }

    "not only waves supported" in {
      upsertRates(eth -> 0.1)
      dex1Api.tryPlace(order) should failWith(9441540, s"Required one of the following fee asset: $EthId, WAVES. But given $BtcId")
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
      waitForOrderAtNode(order.id())
      wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 150L - 50000L)
      wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
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
      waitForOrderAtNode(aliceOrder.id())

      dex1Api.reservedBalance(alice)(eth) shouldBe 960L
      wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 150L - 50000L)
      wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)

      dex1Api.deleteRate(eth)

      val bobSecondOrder = mkBobOrder
      dex1Api.place(bobSecondOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)
      waitForOrderAtNode(bobSecondOrder.id())
      wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 300L - 100000L)
      wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 100000L)
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
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
      waitForOrderAtNode(bobOrder.id())
      wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
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
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)
      dex1Api.reservedBalance(bob).keys should not contain Waves

      val aliceOrder = mkAliceOrder
      dex1Api.place(aliceOrder)

      List(bobOrder, aliceOrder).foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Filled))
      List(bobOrder, aliceOrder).foreach(o => waitForOrderAtNode(o.id()))

      wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
      wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 1920L)
      wavesNode1Api.balance(matcher, eth) shouldBe (matcherEthBalance + 1920L)
      wavesNode1Api.balance(matcher, btc) shouldBe (matcherBtcBalance + 150L)
      wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
      wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)
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
      List(bobOrder, aliceOrder).foreach(o => waitForOrderAtNode(o.id()))

      wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
      wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
      wavesNode1Api.balance(matcher, eth) shouldBe (matcherEthBalance + 960L)

      dex1Api.cancelAll(alice)
      List(btc, eth).foreach(dex1Api.deleteRate)
    }

    "are partial filled both" in {
      val params = Map(9.waves -> 213L, 1900.waves -> 1L, 2000.waves -> 0L)
      for ((aliceOrderAmount, aliceBalanceDiff) <- params) {
        val bobBtcBalance     = wavesNode1Api.balance(bob, btc)
        val aliceBtcBalance   = wavesNode1Api.balance(alice, btc)
        val aliceEthBalance   = wavesNode1Api.balance(alice, eth)
        val matcherEthBalance = wavesNode1Api.balance(matcher, eth)
        val bobWavesBalance   = wavesNode1Api.balance(bob, Waves)
        val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)

        upsertRates(btc -> btcRate, eth -> ethRate)
        val bobOrder = mkBobOrder
        dex1Api.place(bobOrder)
        dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)
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
        List(bobOrder, aliceOrder).foreach(o => waitForOrderAtNode(o.id()))

        wavesNode1Api.balance(bob, btc) shouldBe (bobBtcBalance - 50150L)
        wavesNode1Api.balance(alice, btc) shouldBe (aliceBtcBalance + 50000L)
        wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - aliceBalanceDiff)
        wavesNode1Api.balance(matcher, eth) shouldBe (matcherEthBalance + aliceBalanceDiff)
        wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance + 1.waves)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 1.waves)

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
      List(bobOrder, aliceOrder).foreach(o => waitForOrderAtNode(o.id()))
      dex1Api.cancel(alice, aliceOrder).status shouldBe "OrderCanceled"
      dex1Api.reservedBalance(alice).keys.size shouldBe 0
      wavesNode1Api.balance(alice, eth) shouldBe (aliceEthBalance - 960L)
      List(btc, eth).foreach(dex1Api.deleteRate)
    }
  }

  private def upsertRates(pairs: (IssuedAsset, Double)*): Unit = pairs.foreach {
    case (asset, rate) => withClue(s"$asset")(dex1Api.upsertRate(asset, rate)._1 shouldBe StatusCodes.Created)
  }
}
