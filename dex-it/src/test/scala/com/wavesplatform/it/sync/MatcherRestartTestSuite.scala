package com.wavesplatform.it.sync

import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

class MatcherRestartTestSuite extends NewMatcherSuiteBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx)
  }

  "check order execution" - {
    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Accepted)

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

      val aliceSecondOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant)
      dex1Api.place(aliceSecondOrder)
      dex1Api.waitForOrderStatus(aliceSecondOrder, OrderStatus.Accepted)

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
