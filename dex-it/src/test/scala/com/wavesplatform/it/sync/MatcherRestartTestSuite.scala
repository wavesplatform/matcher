package com.wavesplatform.it.sync

import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.it.MatcherSuiteBase

class MatcherRestartTestSuite extends MatcherSuiteBase {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx, IssueWctTx)
  }

  "check order execution" - {
    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant)
      val bobsOrderToCancel = mkBobOrder(6)
      val bobsOrderToFulfil = mkBobOrder(9)
      val bobsOrders = (1 to 10).map(i => mkBobOrder(i * 10))

      print("place Alice's order")
      placeAndAwaitAtDex(aliceOrder)

      print("place Bob's order and cancel it")
      placeAndAwaitAtDex(bobsOrderToCancel)
      dex1.api.cancelOrderById(bobsOrderToCancel)
      dex1.api.waitForOrderStatus(bobsOrderToCancel, Status.Cancelled)

      print("place Bob's order and fulfil it")
      placeAndAwaitAtDex(bobsOrderToFulfil)
      val aliceSecondOrder = mkOrder(alice, wctWavesPair, OrderType.BUY, bobsOrderToFulfil.amount, 2.waves * Order.PriceConstant)
      placeAndAwaitAtDex(aliceSecondOrder, HttpOrderStatus.Status.Filled)
      eventually {
        dex1.api.waitForOrderStatus(bobsOrderToFulfil, Status.Filled)
      }

      print("place other Bob's orders")
      bobsOrders.foreach(v => placeAndAwaitAtDex(v))

      // Check that order is correct
      print("check Alice's order")
      val orders = dex1.api.getOrderBook(ethWavesPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.waves * Order.PriceConstant

      // Sell order should be in the dex1.api.orderBook
      print("check Alice's orders history")
      dex1.api.getOrderHistoryByPKWithSig(alice).head.status shouldBe Status.Accepted.name

      // Reboot matcher's node
      print("successfully restart matcher")
      dex1.restart()

      print("check Alice's orders")
      dex1.api.waitForOrderStatus(aliceOrder, Status.Accepted)
      dex1.api.getOrderHistoryByPKWithSig(alice).head.status shouldBe Status.Accepted.name

      val orders1 = dex1.api.getOrderBook(ethWavesPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.waves * Order.PriceConstant

      placeAndAwaitAtDex(mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant))

      eventually {
        val orders2 = dex1.api.getOrderBook(ethWavesPair)
        orders2.asks.head.price shouldBe 2.waves * Order.PriceConstant
      }

      val cancel = dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, aliceOrder)
      cancel.status should be("OrderCanceled") // TODO

      val orders3 = dex1.api.getOrderBook(ethWavesPair)
      orders3.asks.head.amount shouldBe 500

      dex1.api.waitForOrderStatus(aliceOrder, Status.Cancelled)
      dex1.api.getOrderHistoryByPKWithSig(alice).head.status shouldBe Status.Accepted.name

      print("check Bob's orders")
      bobsOrders.foreach { order =>
        dex1.api.orderStatusByAssetPairAndId(order).status shouldBe HttpOrderStatus.Status.Accepted
      }

      dex1.api.orderStatusByAssetPairAndId(bobsOrderToFulfil).status shouldBe HttpOrderStatus.Status.Filled
      dex1.api.orderStatusByAssetPairAndId(bobsOrderToCancel).status shouldBe HttpOrderStatus.Status.Cancelled

    }
  }

  private def mkBobOrder(cost: Long) = mkOrder(bob, wctWavesPair, OrderType.SELL, cost, 2.wct * Order.PriceConstant)

}
