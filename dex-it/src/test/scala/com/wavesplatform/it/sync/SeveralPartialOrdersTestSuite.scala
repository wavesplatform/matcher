package com.wavesplatform.it.sync

import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType

class SeveralPartialOrdersTestSuite extends MatcherSuiteBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
  }

  "Alice and Bob trade WAVES-USD" - {
    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val bobWavesBalanceBefore = wavesNode1Api.balance(bob, Waves)

      val bobOrder1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      placeAndAwait(bobOrder1)
      dex1Api.reservedBalance(bob)(Waves) shouldBe sellOrderAmount + matcherFee
      dex1Api.tradableBalance(bob, wavesUsdPair)(Waves) shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      placeAndAwait(aliceOrder, OrderStatus.Filled)

      val aliceOrder2 = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      placeAndAwait(aliceOrder2, OrderStatus.Filled)

      // Bob wants to buy some USD
      dex1Api.waitForOrderStatus(bobOrder1, OrderStatus.Filled)

      // Each side get fair amount of assets
      waitForOrderAtNode(bobOrder1)
      eventually {
        dex1Api.reservedBalance(bob) shouldBe empty
        dex1Api.reservedBalance(alice) shouldBe empty
      }

      // Previously cancelled order should not affect new orders
      val orderBook1 = dex1Api.orderBook(wavesUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      placeAndAwait(bobOrder2)

      val orderBook2 = dex1Api.orderBook(wavesUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty

      dex1Api.cancel(bob, bobOrder2)
      dex1Api.waitForOrderStatus(bobOrder2, OrderStatus.Cancelled)

      dex1Api.reservedBalance(bob) shouldBe empty
      dex1Api.reservedBalance(alice) shouldBe empty
    }

    "place one submitted orders and two counter" in {
      val aliceOrder1 = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      dex1Api.place(aliceOrder1)

      val aliceOrder2 = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      dex1Api.place(aliceOrder2)

      val bobOrder1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      dex1Api.place(bobOrder1)

      dex1Api.waitForOrderStatus(aliceOrder1, OrderStatus.Filled)
      dex1Api.waitForOrderStatus(aliceOrder2, OrderStatus.Filled)
      dex1Api.waitForOrderStatus(bobOrder1, OrderStatus.Filled)

      // Each side get fair amount of assets
      waitForOrderAtNode(bobOrder1)
      eventually {
        dex1Api.reservedBalance(bob) shouldBe empty
        dex1Api.reservedBalance(alice) shouldBe empty
      }

      // Previously cancelled order should not affect new orders
      val orderBook1 = dex1Api.orderBook(wavesUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      placeAndAwait(bobOrder2)

      val orderBook2 = dex1Api.orderBook(wavesUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty
    }
  }
}
