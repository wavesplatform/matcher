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
      val orders = dex1Api.orderBook(ethWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // Sell order should be in the dex1Api.orderBook
      dex1Api.orderHistory(alice).head.status shouldBe OrderStatus.Accepted

      // Reboot matcher's node
      restartContainer(dex1Container(), dex1Api)

      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Accepted)
      dex1Api.orderHistory(alice).head.status shouldBe OrderStatus.Accepted

      val orders1 = dex1Api.orderBook(ethWavesPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant

      placeAndAwait(mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant))

      eventually {
        val orders2 = dex1Api.orderBook(ethWavesPair)
        orders2.asks.head.price shouldBe 2.waves * Order.PriceConstant
      }

      val cancel = dex1Api.cancel(alice, aliceOrder)
      cancel.status should be("OrderCanceled") // TODO

      val orders3 = dex1Api.orderBook(ethWavesPair)
      orders3.asks.head.amount shouldBe 500

      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Cancelled)
      dex1Api.orderHistory(alice).head.status shouldBe OrderStatus.Accepted
    }
  }
}
