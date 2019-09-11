package com.wavesplatform.it.sync

import com.wavesplatform.dex.model.AcceptedOrder
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.FeeConstants._
import com.wavesplatform.it.api.{AssetDecimalsInfo, OrderStatus, OrderStatusResponse}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

import scala.math.BigDecimal.RoundingMode

class TradeBalanceAndRoundingTestSuite extends NewMatcherSuiteBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueEthTx, IssueWctTx)
  }

  "Alice and Bob trade WAVES-USD" - {
    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 3100000000L

    val correctedSellAmount = correctAmount(sellOrderAmount, price)

    val adjustedAmount = receiveAmount(OrderType.BUY, buyOrderAmount, price)
    val adjustedTotal  = receiveAmount(OrderType.SELL, buyOrderAmount, price)

    var aliceWavesBalanceBefore = 0L
    var bobWavesBalanceBefore   = 0L

    "prepare" in {
      log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")
      aliceWavesBalanceBefore = wavesNode1Api.balance(alice, Waves)
      bobWavesBalanceBefore = wavesNode1Api.balance(bob, Waves)
    }

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val bobOrder1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      dex1Api.place(bobOrder1)
      dex1Api.waitForOrderStatus(bobOrder1, OrderStatus.Accepted)
      dex1Api.reservedBalance(bob)(Waves) shouldBe sellOrderAmount + matcherFee
      dex1Api.tradableBalance(bob, wavesUsdPair)(Waves) shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrder(aliceOrder)(_ == OrderStatusResponse(OrderStatus.Filled, Some(420169L)))
      dex1Api.waitForOrder(aliceOrder)(_ == OrderStatusResponse(OrderStatus.Filled, Some(420169L)))

      // Bob wants to buy some USD
      dex1Api.waitForOrder(bobOrder1)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, Some(420169L)))

      // Each side get fair amount of assets
      waitForOrderAtNode(aliceOrder.id())
    }

    "get opened trading markets. USD price-asset" in {
      val openMarkets = dex1Api.allOrderBooks
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe "WAVES"
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(8))

      markets.priceAssetName shouldBe usdAssetName
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
    }

    "check usd and waves balance after fill" in {
      val aliceWavesBalanceAfter = wavesNode1Api.balance(alice, Waves)
      val aliceUsdBalance        = wavesNode1Api.balance(alice, usd)

      val bobWavesBalanceAfter = wavesNode1Api.balance(bob, Waves)
      val bobUsdBalance        = wavesNode1Api.balance(bob, usd)

      (aliceWavesBalanceAfter - aliceWavesBalanceBefore) should be(
        adjustedAmount - (BigInt(matcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())

      aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
      bobWavesBalanceAfter - bobWavesBalanceBefore should be(
        -adjustedAmount - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
      bobUsdBalance should be(adjustedTotal)
    }

    "check filled amount and tradable balance" in {
      val bobOrder     = dex1Api.orderHistory(bob).head
      val filledAmount = dex1Api.orderStatus(bobOrder.assetPair, bobOrder.id).filledAmount.getOrElse(0L)

      filledAmount shouldBe adjustedAmount
    }

    "check reserved balance" in {
      val reservedFee = BigInt(matcherFee) - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount)
      log.debug(s"reservedFee: $reservedFee")
      val expectedBobReservedBalance = correctedSellAmount - adjustedAmount + reservedFee
      dex1Api.reservedBalance(bob)(Waves) shouldBe expectedBobReservedBalance

      dex1Api.reservedBalance(alice) shouldBe empty
    }

    "check waves-usd tradable balance" in {
      val orderHistory = dex1Api.orderHistory(bob)
      orderHistory.size should be(1)

      val expectedBobTradableBalance = bobWavesBalanceBefore - (correctedSellAmount + matcherFee)
      dex1Api.tradableBalance(bob, wavesUsdPair)(Waves) shouldBe expectedBobTradableBalance
      dex1Api.tradableBalance(alice, wavesUsdPair)(Waves) shouldBe wavesNode1Api.balance(alice, Waves)

      val order = orderHistory.head
      dex1Api.cancel(bob, order.assetPair, order.id)
      dex1Api.waitForOrderStatus(order.assetPair, order.id, OrderStatus.Cancelled)
      dex1Api.tradableBalance(bob, order.assetPair)(Waves) shouldBe wavesNode1Api.balance(bob, Waves)
    }
  }

  "Alice and Bob trade WAVES-USD check CELLING" - {
    val price2           = 289
    val buyOrderAmount2  = 0.07.waves
    val sellOrderAmount2 = 3.waves

    val correctedSellAmount2 = correctAmount(sellOrderAmount2, price2)

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val bobWavesBalanceBefore = wavesNode1Api.balance(bob, Waves)
      dex1Api.tradableBalance(bob, wavesUsdPair)(Waves)
      val bobOrder1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount2, price2)
      dex1Api.place(bobOrder1)
      dex1Api.waitForOrderStatus(bobOrder1, OrderStatus.Accepted)

      dex1Api.reservedBalance(bob)(Waves) shouldBe correctedSellAmount2 + matcherFee
      dex1Api.tradableBalance(bob, wavesUsdPair)(Waves) shouldBe bobWavesBalanceBefore - (correctedSellAmount2 + matcherFee)

      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount2, price2)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

      // Bob wants to buy some USD
      dex1Api.waitForOrderStatus(bobOrder1, OrderStatus.PartiallyFilled)

      // Each side get fair amount of assets
      waitForOrderAtNode(aliceOrder.id())
      dex1Api.cancel(bob, bobOrder1)
    }

  }

  "Alice and Bob trade WCT-USD sell price less than buy price" - {
    "place wcd-usd order corrected by new price sell amount less then initial one" in {
      val buyPrice   = 247700
      val sellPrice  = 135600
      val buyAmount  = 46978
      val sellAmount = 56978

      val bobOrder = mkOrder(bob, wctUsdPair, SELL, sellAmount, sellPrice)
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)

      val aliceOrder = mkOrder(alice, wctUsdPair, BUY, buyAmount, buyPrice)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

      waitForOrderAtNode(aliceOrder.id())
      dex1Api.cancel(bob, bobOrder)

      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Cancelled)

      dex1Api.reservedBalance(bob) shouldBe empty
      dex1Api.reservedBalance(alice) shouldBe empty
    }
  }

  "Alice and Bob trade WCT-USD 1" - {
    val wctUsdSellAmount = 347
    val wctUsdBuyAmount  = 146
    val wctUsdPrice      = 12739213

    "place wct-usd order" in {
      val aliceUsdBalance   = wavesNode1Api.balance(alice, usd)
      val bobUsdBalance     = wavesNode1Api.balance(bob, usd)
      val bobWctInitBalance = wavesNode1Api.balance(bob, wct)

      val bobOrder = mkOrder(bob, wctUsdPair, SELL, wctUsdSellAmount, wctUsdPrice)
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)

      val aliceOrder = mkOrder(alice, wctUsdPair, BUY, wctUsdBuyAmount, wctUsdPrice)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

      waitForOrderAtNode(aliceOrder.id())

      val executedAmount         = correctAmount(wctUsdBuyAmount, wctUsdPrice) // 142
      val bobReceiveUsdAmount    = receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)
      val expectedReservedBobWct = wctUsdSellAmount - executedAmount // 205 = 347 - 142

      eventually {
        dex1Api.reservedBalance(bob)(wct) shouldBe expectedReservedBobWct
        // 999999999652 = 999999999999 - 142 - 205
        dex1Api.tradableBalance(bob, wctUsdPair)(wct) shouldBe bobWctInitBalance - executedAmount - expectedReservedBobWct
        dex1Api.tradableBalance(bob, wctUsdPair)(usd) shouldBe bobUsdBalance + bobReceiveUsdAmount
      }

      dex1Api.reservedBalance(alice) shouldBe empty
      dex1Api.tradableBalance(alice, wctUsdPair)(usd) shouldBe aliceUsdBalance - bobReceiveUsdAmount

      val expectedReservedWaves = matcherFee - AcceptedOrder.partialFee(matcherFee, wctUsdSellAmount, executedAmount)
      dex1Api.reservedBalance(bob)(Waves) shouldBe expectedReservedWaves

      dex1Api.cancel(bob, wctUsdPair, dex1Api.orderHistory(bob).head.id)
    }

    "reserved balance is empty after the total execution" in {
      val aliceOrder = mkOrder(alice, wctUsdPair, BUY, 5000000, 100000)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Accepted)

      val bobOrder = mkOrder(bob, wctUsdPair, SELL, 5000000, 99908)
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Filled)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

      waitForOrderAtNode(bobOrder.id())
      eventually {
        dex1Api.reservedBalance(alice) shouldBe empty
        dex1Api.reservedBalance(bob) shouldBe empty
      }
    }
  }

  "get opened trading markets. Check WCT-USD" in {
    val openMarkets = dex1Api.allOrderBooks
    val markets     = openMarkets.markets.last

    markets.amountAssetName shouldBe wctAssetName
    markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))

    markets.priceAssetName shouldBe usdAssetName
    markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
  }

  "Alice and Bob trade WCT-WAVES on not enough fee when place order" - {
    val wctWavesSellAmount = 2
    val wctWavesPrice      = 11234560000000L

    "bob lease all waves exact half matcher fee" in {
      val leasingAmount = wavesNode1Api.balance(bob, Waves) - leasingFee - matcherFee / 2
      val leaseTx       = mkLease(bob, matcher, leasingAmount)
      broadcastAndAwait(leaseTx)

      val bobOrder = mkOrder(bob, wctWavesPair, SELL, wctWavesSellAmount, wctWavesPrice)
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)

      dex1Api.tradableBalance(bob, wctWavesPair)(Waves) shouldBe matcherFee / 2 + receiveAmount(SELL, wctWavesSellAmount, wctWavesPrice) - matcherFee
      dex1Api.cancel(bob, bobOrder)

      dex1Api.tryPlace(mkOrder(bob, wctWavesPair, SELL, wctWavesSellAmount / 2, wctWavesPrice)) should failWith(3147270)

      broadcastAndAwait(mkLeaseCancel(bob, leaseTx.id()))
    }
  }

  "Alice and Bob trade ETH-WAVES" - {
    "reserved balance is empty after the total execution" in {
      val counter1 = mkOrder(alice, ethWavesPair, SELL, 2864310, 300000)
      dex1Api.place(counter1)
      dex1Api.waitForOrderStatus(counter1, OrderStatus.Accepted)

      val counter2 = mkOrder(alice, ethWavesPair, SELL, 7237977, 300000)
      dex1Api.place(counter2)
      dex1Api.waitForOrderStatus(counter2, OrderStatus.Accepted)

      val submitted = mkOrder(bob, ethWavesPair, BUY, 4373667, 300000)
      dex1Api.place(submitted)

      dex1Api.waitForOrderStatus(counter1, OrderStatus.Filled)
      dex1Api.waitForOrderStatus(counter2, OrderStatus.PartiallyFilled)
      dex1Api.waitForOrderStatus(submitted, OrderStatus.Filled)

      waitForOrderAtNode(submitted.id())
      eventually {
        dex1Api.reservedBalance(bob) shouldBe empty
      }
      dex1Api.cancel(alice, counter2)
    }
  }

  "Submitted order Canceled during match" in {
    val bobOrder = mkOrder(matcher, wavesUsdPair, OrderType.SELL, 10000000L, 10L)
    dex1Api.place(bobOrder)
    dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)

    val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100000L, 1000L)
    dex1Api.place(aliceOrder)

    dex1Api.waitForOrder(aliceOrder)(_ == OrderStatusResponse(OrderStatus.Cancelled, Some(0)))

    withClue("Alice's reserved balance:") {
      dex1Api.reservedBalance(alice) shouldBe empty
    }

    val aliceOrders = dex1Api.orderHistoryWithApiKey(alice, activeOnly = Some(false))
    aliceOrders should not be empty

    val order = aliceOrders
      .find(_.id == aliceOrder.id())
      .getOrElse(throw new IllegalStateException(s"Alice should have the ${aliceOrder.id()} order"))

    order.status shouldBe OrderStatus.Cancelled
    dex1Api.cancel(matcher, bobOrder)
  }

  private def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  private def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
}
