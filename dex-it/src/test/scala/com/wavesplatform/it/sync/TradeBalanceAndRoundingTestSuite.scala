//package com.wavesplatform.it.sync
//
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.AssetDecimalsInfo
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.it.util._
//import com.wavesplatform.dex.model.AcceptedOrder
//import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
//import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
//
//import scala.concurrent.duration._
//import scala.math.BigDecimal.RoundingMode
//
//class TradeBalanceAndRoundingTestSuite extends MatcherSuiteBase {
//  {
//    val xs = Seq(IssueUsdTx, IssueEthTx, IssueWctTx).map(_.json()).map(wavesNode1Api.broadcast(_))
//    xs.foreach(x => wavesNode1Api.waitForTransaction(x.id))
//  }
//
//  "Alice and Bob trade WAVES-USD" - {
//    val aliceWavesBalanceBefore = wavesNode1Api.balance(alice, Waves)
//    val bobWavesBalanceBefore   = wavesNode1Api.balance(bob, Waves)
//
//    val price           = 238
//    val buyOrderAmount  = 425532L
//    val sellOrderAmount = 3100000000L
//
//    val correctedSellAmount = correctAmount(sellOrderAmount, price)
//
//    val adjustedAmount = receiveAmount(OrderType.BUY, buyOrderAmount, price)
//    val adjustedTotal  = receiveAmount(OrderType.SELL, buyOrderAmount, price)
//
//    log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")
//
//    "place usd-waves order" in {
//      // Alice wants to sell USD for Waves
//      val bobOrder1   = mkOrder(bob,wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
//      val bobOrder1Id = dex1Api.place(bobOrder1).message.id
//      dex1Api.waitForOrderStatus(bobOrder1Id, OrderStatus.Accepted)
//      dex1Api.reservedBalance(bob)("WAVES") shouldBe sellOrderAmount + matcherFee
//      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)
//
//      val aliceOrder   = mkOrder(alice,wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
//      val aliceOrderId = dex1Api.place(aliceOrder).message.id
//      node.waitOrderStatusAndAmount(wavesUsdPair, aliceOrderId, "Filled", Some(420169L), 1.minute)
//
//      // Bob wants to buy some USD
//      node.waitOrderStatusAndAmount(wavesUsdPair, bobOrder1Id, "PartiallyFilled", Some(420169L), 1.minute)
//
//      // Each side get fair amount of assets
//      waitForOrderAtNode(aliceOrder.idStr())
//    }
//
//    "get opened trading markets. USD price-asset" in {
//      val openMarkets = dex1Api.allOrderBooks()
//      openMarkets.markets.size shouldBe 1
//      val markets = openMarkets.markets.head
//
//      markets.amountAssetName shouldBe "WAVES"
//      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(8))
//
//      markets.priceAssetName shouldBe usdAssetName
//      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
//    }
//
//    "check usd and waves balance after fill" in {
//      val aliceWavesBalanceAfter = wavesNode1Api.balance(alice, Waves)
//      val aliceUsdBalance = wavesNode1Api.balance(alice.toAddress.toString, UsdId.toString).balance
//
//      val bobWavesBalanceAfter = wavesNode1Api.balance(bob, Waves)
//      val bobUsdBalance = wavesNode1Api.balance(bob.toAddress.toString, UsdId.toString).balance
//
//      (aliceWavesBalanceAfter - aliceWavesBalanceBefore) should be(
//        adjustedAmount - (BigInt(matcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())
//
//      aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
//      bobWavesBalanceAfter - bobWavesBalanceBefore should be(
//        -adjustedAmount - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
//      bobUsdBalance should be(adjustedTotal)
//    }
//
//    "check filled amount and tradable balance" in {
//      val bobsOrderId  = dex1Api.orderHistory(bob).head.id
//      val filledAmount = dex1Api.orderStatus(bobsOrderId).filledAmount.getOrElse(0L)
//
//      filledAmount shouldBe adjustedAmount
//    }
//
//    "check reserved balance" in {
//      val reservedFee = BigInt(matcherFee) - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount)
//      log.debug(s"reservedFee: $reservedFee")
//      val expectedBobReservedBalance = correctedSellAmount - adjustedAmount + reservedFee
//      dex1Api.reservedBalance(bob)("WAVES") shouldBe expectedBobReservedBalance
//
//      dex1Api.reservedBalance(alice) shouldBe empty
//    }
//
//    "check waves-usd tradable balance" in {
//      val orderHistory = dex1Api.orderHistory(bob)
//      orderHistory.size should be(1)
//
//      val expectedBobTradableBalance = bobWavesBalanceBefore - (correctedSellAmount + matcherFee)
//      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe expectedBobTradableBalance
//      node.tradableBalance(alice, wavesUsdPair)("WAVES") shouldBe wavesNode1Api.balance(alice, Waves)
//
//      val orderId = orderHistory.head.id
//      node.cancelOrder(bob, wavesUsdPair, orderId)
//      dex1Api.waitForOrderStatus(orderId, OrderStatus.Cancelled)
//      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe wavesNode1Api.balance(bob, Waves)
//    }
//  }
//
//  "Alice and Bob trade WAVES-USD check CELLING" - {
//    val price2           = 289
//    val buyOrderAmount2  = 0.07.waves
//    val sellOrderAmount2 = 3.waves
//
//    val correctedSellAmount2 = correctAmount(sellOrderAmount2, price2)
//
//    "place usd-waves order" in {
//      // Alice wants to sell USD for Waves
//      val bobWavesBalanceBefore = wavesNode1Api.balance(bob, Waves)
//      node.tradableBalance(bob, wavesUsdPair)("WAVES")
//      val bobOrder1   = mkOrder(bob,wavesUsdPair, OrderType.SELL, sellOrderAmount2, price2)
//      val bobOrder1Id = dex1Api.place(bobOrder1).message.id
//      dex1Api.waitForOrderStatus(bobOrder1Id, OrderStatus.Accepted)
//
//      dex1Api.reservedBalance(bob)("WAVES") shouldBe correctedSellAmount2 + matcherFee
//      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (correctedSellAmount2 + matcherFee)
//
//      val aliceOrder   = mkOrder(alice,wavesUsdPair, OrderType.BUY, buyOrderAmount2, price2)
//      val aliceOrderId = dex1Api.place(aliceOrder).message.id
//      dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.Filled)
//
//      // Bob wants to buy some USD
//      dex1Api.waitForOrderStatus(bobOrder1Id, OrderStatus.PartiallyFilled)
//
//      // Each side get fair amount of assets
//      waitForOrderAtNode(aliceOrder.idStr())
//      node.cancelOrder(bob, wavesUsdPair, bobOrder1Id)
//    }
//
//  }
//
//  "Alice and Bob trade WCT-USD sell price less than buy price" - {
//    "place wcd-usd order corrected by new price sell amount less then initial one" in {
//      val buyPrice   = 247700
//      val sellPrice  = 135600
//      val buyAmount  = 46978
//      val sellAmount = 56978
//
//      val bobOrderId = dex1Api.place(mkOrder(bob,wctUsdPair, SELL, sellAmount, sellPrice)).message.id
//      dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Accepted)
//      val aliceOrderId = dex1Api.place(mkOrder(alice,wctUsdPair, BUY, buyAmount, buyPrice)).message.id
//      dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.Filled)
//
//      waitForOrderAtNode(aliceOrderId)
//      node.cancelOrder(bob, wctUsdPair, bobOrderId)
//
//      dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Cancelled)
//
//      dex1Api.reservedBalance(bob) shouldBe empty
//      dex1Api.reservedBalance(alice) shouldBe empty
//    }
//  }
//
//  "Alice and Bob trade WCT-USD 1" - {
//    val wctUsdSellAmount = 347
//    val wctUsdBuyAmount  = 146
//    val wctUsdPrice      = 12739213
//
//    "place wct-usd order" in {
//      val aliceUsdBalance = wavesNode1Api.balance(alice.toAddress.toString, UsdId.toString).balance
//      val bobUsdBalance = wavesNode1Api.balance(bob.toAddress.toString, UsdId.toString).balance
//      val bobWctInitBalance = wavesNode1Api.balance(bob.toAddress.toString, WctId.toString).balance
//
//      val bobOrderId =
//        dex1Api.place(mkOrder(bob,wctUsdPair, SELL, wctUsdSellAmount, wctUsdPrice)).message.id
//      dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Accepted)
//
//      val aliceOrderId =
//        dex1Api.place(mkOrder(alice,wctUsdPair, BUY, wctUsdBuyAmount, wctUsdPrice)).message.id
//      dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.Filled)
//
//      waitForOrderAtNode(aliceOrderId)
//
//      val executedAmount         = correctAmount(wctUsdBuyAmount, wctUsdPrice) // 142
//      val bobReceiveUsdAmount    = receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)
//      val expectedReservedBobWct = wctUsdSellAmount - executedAmount // 205 = 347 - 142
//
//      dex1Api.reservedBalance(bob)(s"$WctId") shouldBe expectedReservedBobWct
//      // 999999999652 = 999999999999 - 142 - 205
//      node.tradableBalance(bob, wctUsdPair)(s"$WctId") shouldBe bobWctInitBalance - executedAmount - expectedReservedBobWct
//      node.tradableBalance(bob, wctUsdPair)(s"$UsdId") shouldBe bobUsdBalance + bobReceiveUsdAmount
//
//      dex1Api.reservedBalance(alice) shouldBe empty
//      node.tradableBalance(alice, wctUsdPair)(s"$UsdId") shouldBe aliceUsdBalance - bobReceiveUsdAmount
//
//      val expectedReservedWaves = matcherFee - AcceptedOrder.partialFee(matcherFee, wctUsdSellAmount, executedAmount)
//      dex1Api.reservedBalance(bob)("WAVES") shouldBe expectedReservedWaves
//
//      node.cancelOrder(bob, wctUsdPair, dex1Api.orderHistory(bob).head.id)
//    }
//
//    "reserved balance is empty after the total execution" in {
//      val aliceOrderId = dex1Api.place(mkOrder(alice,wctUsdPair, BUY, 5000000, 100000)).message.id
//      dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.Accepted)
//
//      val bobOrderId = dex1Api.place(mkOrder(bob,wctUsdPair, SELL, 5000000, 99908)).message.id
//      dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Filled)
//      dex1Api.waitForOrderStatus(aliceOrderId, OrderStatus.Filled)
//
//      waitForOrderAtNode(bobOrderId)
//      dex1Api.reservedBalance(alice) shouldBe empty
//      dex1Api.reservedBalance(bob) shouldBe empty
//    }
//
//  }
//
//  "get opened trading markets. Check WCT-USD" in {
//    val openMarkets = dex1Api.allOrderBooks()
//    val markets     = openMarkets.markets.last
//
//    markets.amountAssetName shouldBe wctAssetName
//    markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
//
//    markets.priceAssetName shouldBe usdAssetName
//    markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
//  }
//
//  "Alice and Bob trade WCT-WAVES on not enough fee when place order" - {
//    val wctWavesSellAmount = 2
//    val wctWavesPrice      = 11234560000000L
//
//    "bob lease all waves exact half matcher fee" in {
//      val leasingAmount = wavesNode1Api.balance(bob, Waves) - leasingFee - matcherFee / 2
//      val leaseTxId     = node.broadcastLease(bob, matcher.toAddress.toString, leasingAmount, leasingFee, waitForTx = true).id
//      val bobOrderId =
//        dex1Api.place(mkOrder(bob,wctWavesPair, SELL, wctWavesSellAmount, wctWavesPrice)).message.id
//      dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Accepted)
//
//      node.tradableBalance(bob, wctWavesPair)("WAVES") shouldBe matcherFee / 2 + receiveAmount(SELL, wctWavesSellAmount, wctWavesPrice) - matcherFee
//      node.cancelOrder(bob, wctWavesPair, bobOrderId)
//
//      assertBadRequestAndResponse(
//        dex1Api.place(mkOrder(bob,wctWavesPair, SELL, wctWavesSellAmount / 2, wctWavesPrice)),
//        "Not enough tradable balance"
//      )
//
//      node.broadcastCancelLease(bob, leaseTxId, leasingFee, waitForTx = true)
//    }
//  }
//
//  "Alice and Bob trade ETH-WAVES" - {
//    "reserved balance is empty after the total execution" in {
//      val counterId1 = dex1Api.place(mkOrder(alice,ethWavesPair, SELL, 2864310, 300000)).message.id
//      dex1Api.waitForOrderStatus(counterId1, OrderStatus.Accepted)
//
//      val counterId2 = dex1Api.place(mkOrder(alice,ethWavesPair, SELL, 7237977, 300000)).message.id
//      dex1Api.waitForOrderStatus(counterId2, OrderStatus.Accepted)
//
//      val submittedId = dex1Api.place(mkOrder(bob,ethWavesPair, BUY, 4373667, 300000)).message.id
//
//      dex1Api.waitForOrderStatus(counterId1, OrderStatus.Filled)
//      dex1Api.waitForOrderStatus(counterId2, OrderStatus.PartiallyFilled)
//      dex1Api.waitForOrderStatus(submittedId, OrderStatus.Filled)
//
//      waitForOrderAtNode(submittedId)
//      dex1Api.reservedBalance(bob) shouldBe empty
//      node.cancelOrder(alice, ethWavesPair, counterId2)
//    }
//  }
//
//  "Submitted order Canceled during match" in {
//    val bobOrder   = mkOrder(matcher,wavesUsdPair, OrderType.SELL, 10000000L, 10L)
//    val bobOrderId = dex1Api.place(bobOrder).message.id
//    dex1Api.waitForOrderStatus(bobOrderId, OrderStatus.Accepted)
//
//    val aliceOrder   = mkOrder(alice,wavesUsdPair, OrderType.BUY, 100000L, 1000L)
//    val aliceOrderId = dex1Api.place(aliceOrder).message.id
//
//    node.waitOrderStatusAndAmount(wavesUsdPair, aliceOrderId, "Cancelled", Some(0), 1.minute)
//
//    withClue("Alice's reserved balance:") {
//      dex1Api.reservedBalance(alice) shouldBe empty
//    }
//
//    val aliceOrders = dex1Api.orderHistoryWithApiKey(alice, activeOnly = false, 1.minute)
//    aliceOrders should not be empty
//
//    val order = aliceOrders.find(_.id == aliceOrderId).getOrElse(throw new IllegalStateException(s"Alice should have the $aliceOrderId order"))
//    order.status shouldBe "Cancelled"
//
//    node.cancelOrder(matcher, wavesUsdPair, bobOrderId)
//  }
//
//  def correctAmount(a: Long, price: Long): Long = {
//    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
//    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
//  }
//
//  def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
//    if (ot == BUY) correctAmount(matchAmount, matchPrice)
//    else {
//      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
//    }
//
//}
