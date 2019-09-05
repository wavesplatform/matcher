package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherMassOrdersTestSuite extends NewMatcherSuiteBase {
  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(s"waves.dex.rest-order-limit = $orderLimit")

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val assets = List(IssueUsdTx, IssueEthTx)
    broadcast(assets: _*)
    assets
      .map(tx => mkTransfer(alice, bob, tx.quantity / 2, IssuedAsset(tx.id())))
      .foreach(wavesNode1Api.broadcast)
  }

  // timeToLive to generate different orders
  private val aliceOrderFill     = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant, ttl = 1.day)
  private val alicePartialOrder  = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant, ttl = 2.days)
  private val aliceOrderToCancel = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant, ttl = 3.days)
  private val aliceActiveOrder   = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant + 100000000)

  "Create orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {
    "Place initial orders" in {
      // Alice places sell orders
      List(aliceOrderFill, alicePartialOrder, aliceOrderToCancel, aliceActiveOrder).foreach(dex1Api.place)

      dex1Api.cancel(alice, aliceOrderToCancel) // TODO: remove this line in DEX-160
      dex1Api.waitForOrderStatus(aliceOrderToCancel, OrderStatus.Cancelled)

      // Bob orders should partially fill one Alice order and fill another
      genAndPlaceOrders(2, bob, ethWavesPair, OrderType.BUY, 2)

      // Check orders after filling
      dex1Api.waitForOrderStatus(alicePartialOrder, OrderStatus.PartiallyFilled)

      dex1Api.waitForOrderStatus(aliceOrderFill, OrderStatus.Filled)
      dex1Api.waitForOrderStatus(alicePartialOrder, OrderStatus.PartiallyFilled)
    }

    "Mass orders creation with random lifetime. Active orders still in list" in {
      dex1Api.orderHistory(alice, activeOnly = Some(false)).length shouldBe 4
      dex1Api.orderHistory(alice, activeOnly = Some(true)).length shouldBe 2

      dex1Api.orderHistory(bob, activeOnly = Some(false)).length shouldBe 2
      dex1Api.orderHistory(bob, activeOnly = Some(true)).length shouldBe 0

      val orderIds = dex1Api.orderHistory(alice).map(_.id)

      orderIds should contain(aliceActiveOrder.id())

      genAndPlaceOrders(orderLimit + 1, alice, wavesUsdPair, OrderType.SELL, 3)

      //wait for some orders cancelled
      Thread.sleep(5000)
      val bobsOrderIds = genAndPlaceOrders(orderLimit + 1, bob, wavesUsdPair, OrderType.BUY, 2)
      Thread.sleep(5000)

      // Alice check that order Active order is still in list
      dex1Api.waitForOrderStatus(aliceActiveOrder, OrderStatus.Accepted)
      dex1Api.waitForOrderStatus(alicePartialOrder, OrderStatus.PartiallyFilled)

      withClue("no flag") {
        val orderIdsAfterMatching1 = dex1Api.orderHistory(alice).map(_.id)
        orderIdsAfterMatching1 should contain(aliceActiveOrder.id())
        orderIdsAfterMatching1 should contain(alicePartialOrder.id())
      }

      withClue("activeOnly=false") {
        val orderIdsAfterMatching2 = dex1Api.orderHistory(alice, activeOnly = Some(false)).map(_.id)
        orderIdsAfterMatching2 should contain(aliceActiveOrder.id())
        orderIdsAfterMatching2 should contain(alicePartialOrder.id())
      }

      withClue("activeOnly=true") {
        val orderIdsAfterMatching3 = dex1Api.orderHistory(alice, activeOnly = Some(true)).map(_.id)
        orderIdsAfterMatching3 should contain(aliceActiveOrder.id())
        orderIdsAfterMatching3 should contain(alicePartialOrder.id())
      }

      dex1Api.orderHistory(bob).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
      dex1Api.orderHistoryByPair(bob, wavesUsdPair).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder =
        dex1Api.orderHistory(alice).lastIndexWhere(o => o.status.equals(OrderStatus.Accepted) || o.status.equals(OrderStatus.PartiallyFilled))
      val firstIdxOfClosedOrder = dex1Api.orderHistory(alice).indexWhere(o => o.status == OrderStatus.Filled || o.status == OrderStatus.Cancelled)
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders =
        dex1Api
          .orderHistory(alice)
          .filter(o => o.status.equals(OrderStatus.Accepted) || o.status.equals(OrderStatus.PartiallyFilled))
          .map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders =
        dex1Api.orderHistory(alice).filter(o => o.status == OrderStatus.Filled || o.status == OrderStatus.Cancelled).map(_.timestamp)
      filledAndCancelledOrders.reverse shouldBe sorted
    }

    "check order history orders count after fill" in {
      val aliceOrderHistory = dex1Api.orderHistory(alice)
      aliceOrderHistory.size shouldBe orderLimit
      val aliceOrderHistoryByPair = dex1Api.orderHistoryByPair(alice, wavesUsdPair)
      aliceOrderHistoryByPair.size shouldBe orderLimit
    }
  }

  private def genAndPlaceOrders(n: Int, sender: KeyPair, assetPair: AssetPair, orderType: OrderType, amount: Long): Seq[Order.Id] =
    (1 to n).map { _ =>
      val order = mkOrder(sender, assetPair, orderType, amount, Order.PriceConstant, ttl = (120 + Random.nextInt(70)).seconds)
      dex1Api.place(order)
      order.id()
    }

}
