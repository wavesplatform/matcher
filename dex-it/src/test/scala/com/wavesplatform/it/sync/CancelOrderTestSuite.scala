package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import mouse.any.anySyntaxMouse

class CancelOrderTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "Order can be canceled" - {

    "After cancelAllOrders (200) all of them should be cancelled" in {
      val time = System.currentTimeMillis

      val orders = (1 to 200) map { i =>
        mkOrder(bob, wavesBtcPair, OrderType.SELL, 1000000, 123450000L, 300000, version = 2: Byte, ts = time + i) unsafeTap { order =>
          placeAndAwait(order)
        }
      }

      dex1.api.cancelAll(bob)

      orders.foreach(order => dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled))
    }

    "by sender" in {
      val order = mkBobOrder
      placeAndAwait(order)

      dex1.api.cancel(bob, order)
      dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)

      dex1.api.orderHistoryByPair(bob, wavesUsdPair).collectFirst {
        case o if o.id == order.id() => o.status shouldEqual OrderStatus.Cancelled
      }
    }

    "with API key" in {
      val order = mkBobOrder
      placeAndAwait(order)

      dex1.api.cancelWithApiKey(order)
      dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)

      dex1.api.orderHistory(bob).find(_.id == order.id()).get.status shouldBe OrderStatus.Cancelled
      dex1.api.orderHistoryByPair(bob, wavesUsdPair).find(_.id == order.id()).get.status shouldBe OrderStatus.Cancelled

      val orderBook = dex1.api.orderBook(wavesUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val order = mkBobOrder
      placeAndAwait(order)

      val r = dex1.api.tryCancel(matcher, order)
      r shouldBe 'left
      r.left.get.error shouldBe 9437193 // TODO

      // Cleanup
      dex1.api.cancel(bob, order)
      dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)
    }
  }

  "Batch cancel works for" - {
    "all orders placed by an address" in {
      val orders = mkBobOrders(wavesUsdPair) ::: mkBobOrders(wavesBtcPair)
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))

      dex1.api.cancelAll(bob)

      orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Cancelled))
    }

    "a pair" in {
      val wavesUsdOrders = mkBobOrders(wavesUsdPair)
      val wavesBtcOrders = mkBobOrders(wavesBtcPair)
      val orders         = wavesUsdOrders ::: wavesBtcOrders
      orders.foreach(dex1.api.place)
      orders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))

      dex1.api.cancelAllByPair(bob, wavesBtcPair)

      wavesBtcOrders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Cancelled))
      wavesUsdOrders.foreach(dex1.api.waitForOrderStatus(_, OrderStatus.Accepted))
    }
  }

  private def mkBobOrder                        = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 800)
  private def mkBobOrders(assetPair: AssetPair) = (1 to 5).map(i => mkOrder(bob, assetPair, OrderType.SELL, 100.waves + i, 400)).toList
}
