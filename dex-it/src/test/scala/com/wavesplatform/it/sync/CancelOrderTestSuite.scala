package com.wavesplatform.it.sync

import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.OrderStatus
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

class CancelOrderTestSuite extends MatcherSuiteBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
  }

  "Order can be canceled" - {
    "by sender" in {
      val order = mkBobOrder
      placeAndAwait(order)

      dex1Api.cancel(bob, order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Cancelled)

      dex1Api.orderHistoryByPair(bob, wavesUsdPair).collectFirst {
        case o if o.id == order.id() => o.status shouldEqual OrderStatus.Cancelled
      }
    }

    "with API key" in {
      val order = mkBobOrder
      placeAndAwait(order)

      dex1Api.cancelWithApiKey(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Cancelled)

      dex1Api.orderHistory(bob).find(_.id == order.id()).get.status shouldBe OrderStatus.Cancelled
      dex1Api.orderHistoryByPair(bob, wavesUsdPair).find(_.id == order.id()).get.status shouldBe OrderStatus.Cancelled

      val orderBook = dex1Api.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val order = mkBobOrder
      placeAndAwait(order)

      val r = dex1Api.tryCancel(matcher, order)
      r shouldBe 'left
      r.left.get.error shouldBe 9437193 // TODO

      // Cleanup
      dex1Api.cancel(bob, order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Cancelled)
    }
  }

  "Batch cancel works for" - {
    "all orders placed by an address" in {
      val orders = mkBobOrders(wavesUsdPair) ::: mkBobOrders(wavesBtcPair)
      orders.foreach(dex1Api.place)
      orders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Accepted))

      dex1Api.cancelAll(bob)

      orders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Cancelled))
    }

    "a pair" in {
      val wavesUsdOrders = mkBobOrders(wavesUsdPair)
      val wavesBtcOrders = mkBobOrders(wavesBtcPair)
      val orders         = wavesUsdOrders ::: wavesBtcOrders
      orders.foreach(dex1Api.place)
      orders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Accepted))

      dex1Api.cancelAllByPair(bob, wavesBtcPair)

      wavesBtcOrders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Cancelled))
      wavesUsdOrders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Accepted))
    }
  }

  private def mkBobOrder                        = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800)
  private def mkBobOrders(assetPair: AssetPair) = (1 to 5).map(i => mkOrder(bob, assetPair, OrderType.SELL, 100.waves + i, 400)).toList
}
