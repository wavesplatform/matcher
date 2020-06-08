package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.ApiLevelAgg
import com.wavesplatform.dex.api.ApiOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.MatcherSuiteBase

class SeveralPartialOrdersTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "Alice and Bob trade WAVES-USD" - {
    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place usd-waves order" in {
      // Alice wants to sell USD for Waves
      val bobWavesBalanceBefore = wavesNode1.api.balance(bob, Waves)

      val bobOrder1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      placeAndAwaitAtDex(bobOrder1)
      dex1.api.reservedBalance(bob)(Waves) shouldBe sellOrderAmount + matcherFee
      dex1.api.tradableBalance(bob, wavesUsdPair)(Waves) shouldBe bobWavesBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      placeAndAwaitAtDex(aliceOrder, Status.Filled)

      val aliceOrder2 = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      placeAndAwaitAtDex(aliceOrder2, Status.Filled)

      // Bob wants to buy some USD
      dex1.api.waitForOrderStatus(bobOrder1, Status.Filled)

      // Each side get fair amount of assets
      waitForOrderAtNode(bobOrder1)
      eventually {
        dex1.api.reservedBalance(bob) shouldBe empty
        dex1.api.reservedBalance(alice) shouldBe empty
      }

      // Previously cancelled order should not affect new orders
      val orderBook1 = dex1.api.orderBook(wavesUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      placeAndAwaitAtDex(bobOrder2)

      val orderBook2 = dex1.api.orderBook(wavesUsdPair)
      orderBook2.asks shouldBe List(ApiLevelAgg(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty

      dex1.api.cancel(bob, bobOrder2)
      dex1.api.waitForOrderStatus(bobOrder2, Status.Cancelled)

      dex1.api.reservedBalance(bob) shouldBe empty
      dex1.api.reservedBalance(alice) shouldBe empty
    }

    "place one submitted orders and two counter" in {
      val aliceOrder1 = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      dex1.api.place(aliceOrder1)

      val aliceOrder2 = mkOrder(alice, wavesUsdPair, OrderType.BUY, buyOrderAmount, price)
      dex1.api.place(aliceOrder2)

      val bobOrder1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      dex1.api.place(bobOrder1)

      dex1.api.waitForOrderStatus(aliceOrder1, Status.Filled)
      dex1.api.waitForOrderStatus(aliceOrder2, Status.Filled)
      dex1.api.waitForOrderStatus(bobOrder1, Status.Filled)

      // Each side get fair amount of assets
      waitForOrderAtNode(bobOrder1)
      eventually {
        dex1.api.reservedBalance(bob) shouldBe empty
        dex1.api.reservedBalance(alice) shouldBe empty
      }

      // Previously cancelled order should not affect new orders
      val orderBook1 = dex1.api.orderBook(wavesUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, sellOrderAmount, price)
      placeAndAwaitAtDex(bobOrder2)

      val orderBook2 = dex1.api.orderBook(wavesUsdPair)
      orderBook2.asks shouldBe List(ApiLevelAgg(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty
    }
  }
}
