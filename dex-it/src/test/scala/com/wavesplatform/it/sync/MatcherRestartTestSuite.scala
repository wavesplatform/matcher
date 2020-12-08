package com.wavesplatform.it.sync

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.it.MatcherSuiteBase

class MatcherRestartTestSuite extends MatcherSuiteBase {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx)
  }

  "check order execution" - {
    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant)
      placeAndAwaitAtDex(aliceOrder)

      // Check that order is correct
      val orders = dex1.api.getOrderBook(ethWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // Sell order should be in the dex1.api.orderBook
      dex1.api.getOrderHistoryByPublicKey(alice).head.status shouldBe Status.Accepted.name

      // Reboot matcher's node
      dex1.restart()

      dex1.api.waitForOrderStatus(aliceOrder, Status.Accepted)
      dex1.api.getOrderHistoryByPublicKey(alice).head.status shouldBe Status.Accepted.name

      val orders1 = dex1.api.getOrderBook(ethWavesPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant

      placeAndAwaitAtDex(mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant))

      eventually {
        val orders2 = dex1.api.getOrderBook(ethWavesPair)
        orders2.asks.head.price shouldBe 2.waves * Order.PriceConstant
      }

      val cancel = dex1.api.cancelOrder(alice, aliceOrder)
      cancel.status should be("OrderCanceled") // TODO

      val orders3 = dex1.api.getOrderBook(ethWavesPair)
      orders3.asks.head.amount shouldBe 500

      dex1.api.waitForOrderStatus(aliceOrder, Status.Cancelled)
      dex1.api.getOrderHistoryByPublicKey(alice).head.status shouldBe Status.Accepted.name
    }
  }
}
