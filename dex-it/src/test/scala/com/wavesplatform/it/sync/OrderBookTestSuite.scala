//package com.wavesplatform.it.sync
//
//import com.wavesplatform.account.KeyPair
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
//import com.wavesplatform.transaction.assets.exchange.OrderType._
//
//class OrderBookTestSuite extends MatcherSuiteBase {
//
//  {
//    val xs = Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(wavesNode1Api.broadcast(_))
//    xs.foreach(x => wavesNode1Api.waitForTransaction(x.id))
//    node.waitForHeight(node.height + 1)
//  }
//
//  case class ReservedBalances(wct: Long, usd: Long, waves: Long)
//  def reservedBalancesOf(pk: KeyPair): ReservedBalances = {
//    val reservedBalances = node.reservedBalance(pk)
//    ReservedBalances(
//      reservedBalances.getOrElse(WctId.toString, 0),
//      reservedBalances.getOrElse(UsdId.toString, 0),
//      reservedBalances.getOrElse("WAVES", 0)
//    )
//  }
//
//  val (amount, price) = (1000L, PriceConstant)
//
//  "When delete order book" - {
//    val buyOrder        = dex1Api.place(mkOrder(alice, matcher,wctUsdPair, BUY, 2 * amount, price)).message.id
//    val anotherBuyOrder = dex1Api.place(mkOrder(alice, matcher,wctUsdPair, BUY, amount, price)).message.id
//
//    val submitted = dex1Api.place(mkOrder(bob, matcher,wctUsdPair, SELL, amount, price)).message.id
//
//    val sellOrder = dex1Api.place(mkOrder(bob, matcher,wctUsdPair, SELL, amount, 2 * price)).message.id
//
//    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
//    dex1Api.waitForOrderStatus(submitted, OrderStatus.Filled)
//
//    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
//
//    val buyOrderForAnotherPair = dex1Api.place(mkOrder(alice, matcher,wctWavesPair, BUY, amount, price)).message.id
//    val sellOrderForAnotherPair =
//      dex1Api.place(mkOrder(bob, matcher,wctWavesPair, SELL, amount, 2 * price)).message.id
//
//    dex1Api.waitForOrderStatus(buyOrderForAnotherPair, OrderStatus.Accepted)
//    dex1Api.waitForOrderStatus(sellOrderForAnotherPair, OrderStatus.Accepted)
//
//    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
//
//    node.deleteOrderBook(wctUsdPair)
//
//    "orders by the pair should be canceled" in {
//      dex1Api.waitForOrderStatus(buyOrder, OrderStatus.Cancelled)
//      dex1Api.waitForOrderStatus(anotherBuyOrder, OrderStatus.Cancelled)
//      dex1Api.waitForOrderStatus(sellOrder, OrderStatus.Cancelled)
//    }
//
//    "orderbook was deleted" in {
//      withClue("orderBook") {
//        val orderBook = node.orderBook(wctUsdPair)
//        orderBook.bids shouldBe empty
//        orderBook.asks shouldBe empty
//      }
//
//      withClue("tradingMarkets") {
//        val tradingPairs = node.tradingMarkets().markets.map(x => s"${x.amountAsset}-${x.priceAsset}")
//        tradingPairs shouldNot contain(wctUsdPair.key)
//      }
//
//      withClue("getAllSnapshotOffsets") {
//        dex1Api.allSnapshotOffsets.keySet shouldNot contain(wctUsdPair.key)
//      }
//    }
//
//    "reserved balances should be released for the pair" in {
//      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
//      aliceReservedBalances.usd shouldBe 0
//      aliceReservedBalances.waves shouldBe (aliceRBForBothPairs.waves - aliceRBForOnePair.waves)
//      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
//      bobReservedBalances.waves shouldBe (bobRBForBothPairs.waves - bobRBForOnePair.waves)
//    }
//
//    "it should not affect other pairs and their orders" in {
//      dex1Api.orderStatus(buyOrderForAnotherPair).status shouldBe "Accepted"
//      dex1Api.orderStatus(sellOrderForAnotherPair).status shouldBe "Accepted"
//      dex1Api.place(mkOrder(alice, matcher,wctWavesPair, BUY, amount, price))
//
//      val orderBook = node.orderBook(wctWavesPair)
//      orderBook.bids shouldNot be(empty)
//      orderBook.asks shouldNot be(empty)
//    }
//
//    "matcher can start after multiple delete events" in {
//      import com.wavesplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}
//
//      def deleteWctWaves = async(node).deleteOrderBook(wctWavesPair)
//      val deleteMultipleTimes = deleteWctWaves
//        .zip(deleteWctWaves)
//        .map(_ => ())
//        .recover { case _ => () } // It's ok: either this should fail, or restartNode should work
//
//      SyncMatcherHttpApi.sync(deleteMultipleTimes)
//
//      docker.restartNode(node)
//    }
//  }
//}
