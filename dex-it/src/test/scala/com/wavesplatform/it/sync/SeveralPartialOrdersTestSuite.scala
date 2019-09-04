//package com.wavesplatform.it.sync
//
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.LevelResponse
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.transaction.assets.exchange.OrderType.BUY
//import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
//
//import scala.concurrent.duration._
//import scala.math.BigDecimal.RoundingMode
//
//class SeveralPartialOrdersTestSuite extends MatcherSuiteBase {
//  override protected def beforeAll(): Unit = {
//    super.beforeAll()
//    wavesNode1Api.waitForTransaction(wavesNode1Api.broadcast(IssueUsdTx).id)
//  }
//
//  "Alice and Bob trade WAVES-USD" - {
//    val price           = 238
//    val buyOrderAmount  = 425532L
//    val sellOrderAmount = 840340L
//
//    "place usd-waves order" in {
//      // Alice wants to sell USD for Waves
//      val bobWavesBalanceBefore = node.accountBalances(bob.toAddress.toString)._1
//
//      val bobOrder1   = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
//      val bobOrder1Id = dex1Api.place(bobOrder1).message.id
//      node.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Accepted", 1.minute)
//      dex1Api.reservedBalance(bob)("WAVES") shouldBe sellOrderAmount + matcherFee
//      node.tradableBalance(bob, wavesUsdPair)("WAVES") shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)
//
//      val aliceOrder   = mkOrder(alice, matcher,wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
//      val aliceOrderId = dex1Api.place(aliceOrder).message.id
//      node.waitOrderStatus(wavesUsdPair, aliceOrderId, "Filled", 1.minute)
//
//      val aliceOrder2   = mkOrder(alice, matcher,wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
//      val aliceOrder2Id = dex1Api.place(aliceOrder2).message.id
//      node.waitOrderStatus(wavesUsdPair, aliceOrder2Id, "Filled", 1.minute)
//
//      // Bob wants to buy some USD
//      node.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Filled", 1.minute)
//
//      // Each side get fair amount of assets
//      node.waitOrderInBlockchain(bobOrder1Id)
//      dex1Api.reservedBalance(bob) shouldBe empty
//      dex1Api.reservedBalance(alice) shouldBe empty
//
//      // Previously cancelled order should not affect new orders
//      val orderBook1 = dex1Api.orderBook(wavesUsdPair)
//      orderBook1.asks shouldBe empty
//      orderBook1.bids shouldBe empty
//
//      val bobOrder2   = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
//      val bobOrder2Id = dex1Api.place(bobOrder2).message.id
//      node.waitOrderStatus(wavesUsdPair, bobOrder2Id, "Accepted", 1.minute)
//
//      val orderBook2 = dex1Api.orderBook(wavesUsdPair)
//      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
//      orderBook2.bids shouldBe empty
//
//      node.cancelOrder(bob, wavesUsdPair, bobOrder2Id)
//      node.waitOrderStatus(wavesUsdPair, bobOrder2Id, "Cancelled", 1.minute)
//
//      dex1Api.reservedBalance(bob) shouldBe empty
//      dex1Api.reservedBalance(alice) shouldBe empty
//    }
//
//    "place one submitted orders and two counter" in {
//      val aliceOrder1   = mkOrder(alice, matcher,wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
//      val aliceOrder1Id = dex1Api.place(aliceOrder1).message.id
//
//      val aliceOrder2   = mkOrder(alice, matcher,wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
//      val aliceOrder2Id = dex1Api.place(aliceOrder2).message.id
//
//      val bobOrder1   = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
//      val bobOrder1Id = dex1Api.place(bobOrder1).message.id
//
//      node.waitOrderStatus(wavesUsdPair, aliceOrder1Id, "Filled", 1.minute)
//      node.waitOrderStatus(wavesUsdPair, aliceOrder2Id, "Filled", 1.minute)
//      node.waitOrderStatus(wavesUsdPair, bobOrder1Id, "Filled", 1.minute)
//
//      // Each side get fair amount of assets
//      node.waitOrderInBlockchain(bobOrder1Id)
//      dex1Api.reservedBalance(bob) shouldBe empty
//      dex1Api.reservedBalance(alice) shouldBe empty
//
//      // Previously cancelled order should not affect new orders
//      val orderBook1 = dex1Api.orderBook(wavesUsdPair)
//      orderBook1.asks shouldBe empty
//      orderBook1.bids shouldBe empty
//
//      val bobOrder2   = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
//      val bobOrder2Id = dex1Api.place(bobOrder2).message.id
//      node.waitOrderStatus(wavesUsdPair, bobOrder2Id, "Accepted", 1.minute)
//
//      val orderBook2 = dex1Api.orderBook(wavesUsdPair)
//      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
//      orderBook2.bids shouldBe empty
//    }
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
