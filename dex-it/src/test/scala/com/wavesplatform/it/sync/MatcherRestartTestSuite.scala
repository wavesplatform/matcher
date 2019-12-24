package com.wavesplatform.it.sync

import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

class MatcherRestartTestSuite extends MatcherSuiteBase {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx)
  }

  "check order execution" - {
    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant)
      placeAndAwait(aliceOrder)

      // Check that order is correct
      val orders = dex1.api.orderBook(ethWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // Sell order should be in the dex1.api.orderBook
      dex1.api.orderHistory(alice).head.status shouldBe OrderStatus.Accepted

      // Reboot matcher's node
      dex1.restart()

      dex1.api.waitForOrderStatus(aliceOrder, OrderStatus.Accepted)
      dex1.api.orderHistory(alice).head.status shouldBe OrderStatus.Accepted

      val orders1 = dex1.api.orderBook(ethWavesPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant

      placeAndAwait(mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant))

      eventually {
        val orders2 = dex1.api.orderBook(ethWavesPair)
        orders2.asks.head.price shouldBe 2.waves * Order.PriceConstant
      }

      val cancel = dex1.api.cancel(alice, aliceOrder)
      cancel.status should be("OrderCanceled") // TODO

      val orders3 = dex1.api.orderBook(ethWavesPair)
      orders3.asks.head.amount shouldBe 500

      dex1.api.waitForOrderStatus(aliceOrder, OrderStatus.Cancelled)
      dex1.api.orderHistory(alice).head.status shouldBe OrderStatus.Accepted
    }
  }
}
