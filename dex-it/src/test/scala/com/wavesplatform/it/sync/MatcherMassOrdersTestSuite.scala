package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.it.MatcherSuiteBase

import scala.concurrent.duration._

class MatcherMassOrdersTestSuite extends MatcherSuiteBase {

  private val maxActiveOrders = 20
  private val maxFinalizedOrders = 10

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  address-actor.max-active-orders = $maxActiveOrders
       |  order-db.max-orders = $maxFinalizedOrders
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()

    val assets = List(IssueUsdTx, IssueEthTx)
    broadcastAndAwait(assets: _*)
    assets
      .map(tx => mkTransfer(alice, bob, tx.quantity() / 2, IssuedAsset(tx.id())))
      .foreach(wavesNode1.api.broadcast)

    dex1.start()
  }

  // timeToLive to generate different orders
  private val aliceOrderFill = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant, ttl = 1.day)
  private val alicePartialOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant, ttl = 2.days)
  private val aliceOrderToCancel = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant, ttl = 3.days)
  private val aliceActiveOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 3, Order.PriceConstant + 100000000)

  "Create orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {
    "Place initial orders" in {
      // Alice places sell orders
      List(aliceOrderFill, alicePartialOrder, aliceOrderToCancel, aliceActiveOrder).foreach(dex1.api.place)

      dex1.api.cancelOrder(alice, aliceOrderToCancel) // TODO: remove this line in DEX-160
      dex1.api.waitForOrderStatus(aliceOrderToCancel, Status.Cancelled)

      // Bob orders should partially fill one Alice order and fill another
      genAndPlaceOrders(2, bob, ethWavesPair, OrderType.BUY, 2)

      // Check orders after filling
      dex1.api.waitForOrderStatus(alicePartialOrder, Status.PartiallyFilled)

      dex1.api.waitForOrderStatus(aliceOrderFill, Status.Filled)
      dex1.api.waitForOrderStatus(alicePartialOrder, Status.PartiallyFilled)
    }

    "Mass orders creation with random lifetime. Active orders still in list" in {
      val activeAliceOrders1 = 2

      dex1.api.getOrderHistoryByPublicKey(alice, activeOnly = Some(false)).length shouldBe 4
      dex1.api.getOrderHistoryByPublicKey(alice, activeOnly = Some(true)).length shouldBe activeAliceOrders1

      dex1.api.getOrderHistoryByPublicKey(bob, activeOnly = Some(false)).length shouldBe 2
      dex1.api.getOrderHistoryByPublicKey(bob, activeOnly = Some(true)).length shouldBe 0

      val orderIds = dex1.api.getOrderHistoryByPublicKey(alice).map(_.id)

      orderIds should contain(aliceActiveOrder.id())

      genAndPlaceOrders(maxActiveOrders - activeAliceOrders1, alice, wavesUsdPair, OrderType.SELL, 3)
      val bobsOrderIds = genAndPlaceOrders(maxActiveOrders, bob, wavesUsdPair, OrderType.BUY, 2).reverse
      bobsOrderIds.foreach(dex1.api.waitForOrderStatus(wavesUsdPair, _, Status.Filled))

      // Alice check that order Active order is still in list
      dex1.api.waitForOrderStatus(aliceActiveOrder, Status.Accepted)
      dex1.api.waitForOrderStatus(alicePartialOrder, Status.PartiallyFilled)

      withClue("no flag") {
        val orderIdsAfterMatching1 = dex1.api.getOrderHistoryByPublicKey(alice).map(_.id)
        orderIdsAfterMatching1 should contain(aliceActiveOrder.id())
        orderIdsAfterMatching1 should contain(alicePartialOrder.id())
      }

      withClue("activeOnly=false") {
        val orderIdsAfterMatching2 = dex1.api.getOrderHistoryByPublicKey(alice, activeOnly = Some(false)).map(_.id)
        orderIdsAfterMatching2 should contain(aliceActiveOrder.id())
        orderIdsAfterMatching2 should contain(alicePartialOrder.id())
      }

      withClue("activeOnly=true") {
        val orderIdsAfterMatching3 = dex1.api.getOrderHistoryByPublicKey(alice, activeOnly = Some(true)).map(_.id)
        orderIdsAfterMatching3 should contain(aliceActiveOrder.id())
        orderIdsAfterMatching3 should contain(alicePartialOrder.id())
      }

      // All orders are finalized
      dex1.api.getOrderHistoryByPublicKey(bob).map(_.id).take(maxFinalizedOrders) should matchTo(bobsOrderIds.take(maxFinalizedOrders))
      dex1.api.getOrderHistoryByAssetPairAndPublicKey(bob, wavesUsdPair).map(_.id).take(maxFinalizedOrders) should matchTo(
        bobsOrderIds.take(maxFinalizedOrders)
      )
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder =
        dex1.api.getOrderHistoryByPublicKey(alice).lastIndexWhere(o =>
          o.status == Status.Accepted.name || o.status == Status.PartiallyFilled.name
        )
      val firstIdxOfClosedOrder =
        dex1.api.getOrderHistoryByPublicKey(alice).indexWhere(o => o.status == Status.Filled.name || o.status == Status.Cancelled.name)
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders =
        dex1.api
          .getOrderHistoryByPublicKey(alice)
          .filter(o => o.status == Status.Accepted.name || o.status == Status.PartiallyFilled.name)
          .map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders =
        dex1.api.getOrderHistoryByPublicKey(alice).filter(o => o.status === Status.Filled.name || o.status == Status.Cancelled.name).map(
          _.timestamp
        )
      filledAndCancelledOrders.reverse shouldBe sorted
    }

    "check order history orders count after fill" in {
      val aliceAllActiveOrders = dex1.api.getOrderHistoryByPublicKey(alice, activeOnly = Some(true))
      val aliceAllOrders = dex1.api.getOrderHistoryByPublicKey(alice)
      val expectedAllOrders = maxFinalizedOrders + aliceAllActiveOrders.size
      aliceAllOrders.size shouldBe expectedAllOrders

      val alicePairActiveOrders = dex1.api.getOrderHistoryByAssetPairAndPublicKey(alice, wavesUsdPair, activeOnly = Some(true))
      val alicePairOrders = dex1.api.getOrderHistoryByAssetPairAndPublicKey(alice, wavesUsdPair)
      val expectedPairOrders = maxFinalizedOrders + alicePairActiveOrders.size
      alicePairOrders.size shouldBe expectedPairOrders
    }
  }

  private def genAndPlaceOrders(n: Int, sender: KeyPair, assetPair: AssetPair, orderType: OrderType, amount: Long): List[Order.Id] =
    (1 to n).map { i =>
      val order = mkOrder(sender, assetPair, orderType, amount, Order.PriceConstant, ttl = (120 + i).seconds)
      dex1.api.place(order)
      order.id()
    }.toList

}
