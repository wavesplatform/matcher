package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.ApiOrderStatus.Status
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.it.MatcherSuiteBase

class RestOrderLimitTestSuite extends MatcherSuiteBase {

  private val maxActiveOrders    = 50
  private val maxFinalizedOrders = 3

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  address-actor.max-active-orders = $maxActiveOrders
       |  order-db.max-orders = $maxFinalizedOrders
       |}""".stripMargin
  )

  private def activeOrders: List[Order.Id] = {
    val activeOrders = dex1.api.orderHistory(alice, activeOnly = Some(true)).map(_.id)
    dex1.api.orderHistoryWithApiKey(alice, activeOnly = Some(true)).map(_.id) should matchTo(activeOrders)
    activeOrders
  }

  private def allOrders: List[Order.Id] = {
    val allOrders = dex1.api.orderHistory(alice).map(_.id)
    dex1.api.orderHistoryWithApiKey(alice, activeOnly = Some(false)).map(_.id) should matchTo(allOrders)
    allOrders
  }

  private def activeOrdersBy(pair: AssetPair, n: KeyPair = alice): List[Order.Id] =
    dex1.api.orderHistoryByPair(n, pair, activeOnly = Some(true)).map(_.id)

  private def allOrdersBy(pair: AssetPair, n: KeyPair = alice): List[Order.Id] = dex1.api.orderHistoryByPair(n, pair).map(_.id)

  markup("""Test suite checks only Alice's OrderHistory.
           |Bob places orders only for matching Alice's orders.""".stripMargin)

  "Order History REST API methods should have limit for orders in response" in {
    val now = System.currentTimeMillis()

    val IssueResults(issueAliceAssetTx, _, aliceAsset) = mkIssueExtended(alice, "AliceCoin", someAssetAmount, 0)
    val IssueResults(issueBobAssetTx, _, bobAsset)     = mkIssueExtended(bob, "BobCoin", someAssetAmount, 0)

    broadcastAndAwait(issueAliceAssetTx, issueBobAssetTx)

    val alicePair = AssetPair(aliceAsset, Waves)
    val bobPair   = AssetPair(bobAsset, Waves)

    info("'fullOrderHistory' and 'ordersByAddress' (activeOnly=false) must return no more 'rest-order-limit' orders")

    val active0    = mkOrder(alice, alicePair, SELL, 1, 15.waves, ts = now)
    val active1    = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 1)
    val partial1   = mkOrder(alice, alicePair, SELL, 2, 9.waves, ts = now + 2)
    val filled1    = mkOrder(alice, alicePair, SELL, 1, 8.waves, ts = now + 3)
    val cancelled1 = mkOrder(alice, alicePair, SELL, 1, 11.waves, ts = now + 4)
    val active2    = mkOrder(alice, bobPair, BUY, 1, 2.waves, ts = now + 5)
    val filled2    = mkOrder(alice, bobPair, BUY, 1, 4.waves, ts = now + 6)
    val partial2   = mkOrder(alice, bobPair, BUY, 2, 3.waves, ts = now + 7)
    val cancelled2 = mkOrder(alice, bobPair, BUY, 1, 2.waves, ts = now + 8)
    List(active0, active1, partial1, filled1, cancelled1, active2, filled2, partial2, cancelled2).foreach(dex1.api.place)

    // orders for matching Alice's orders
    List(
      mkOrder(bob, alicePair, BUY, 1, 8.waves, ts = now + 9), // fill filled1
      mkOrder(bob, alicePair, BUY, 1, 9.waves, ts = now + 10), // part fill partial1
      mkOrder(bob, bobPair, SELL, 1, 4.waves, ts = now + 11), // fill filled2
      mkOrder(bob, bobPair, SELL, 1, 3.waves, ts = now + 12) // part fill partial2
    ).foreach(dex1.api.place)

    dex1.api.cancel(alice, cancelled1)
    dex1.api.cancel(alice, cancelled2)
    dex1.api.waitForOrderStatus(cancelled2, Status.Cancelled)

    val activeOrdersAllFive       = List(partial2, active2, partial1, active1, active0).map(_.id())
    val allOrdersExceptTheFilled1 = activeOrdersAllFive ++ Seq(cancelled2, filled2, cancelled1).map(_.id())
    val activeOrdersByPair        = List(partial1, active1, active0).map(_.id())
    val allOrdersByPair           = activeOrdersByPair ++ Seq(cancelled1, filled1).map(_.id())

    activeOrders should matchTo(activeOrdersAllFive)
    allOrders should matchTo(allOrdersExceptTheFilled1)
    activeOrdersBy(alicePair) should matchTo(activeOrdersByPair)
    allOrdersBy(alicePair) should matchTo(allOrdersByPair)

    info("'fullOrderHistory' and 'ordersByAddress' must return all active orders, even if they are more than the limit")

    val active3 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 13)
    val active4 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 14)
    val active5 = mkOrder(alice, bobPair, BUY, 1, 2.waves, ts = now + 15)
    val active6 = mkOrder(alice, bobPair, BUY, 1, 2.waves, ts = now + 16)
    List(active3, active4, active5, active6).foreach(dex1.api.place)

    dex1.api.waitForOrderStatus(active6, Status.Accepted)

    val activeOrdersAllNine          = List(active6, active5, active4, active3).map(_.id()) ++ activeOrdersAllFive
    val activeOrdersByPairWithTwoNew = List(active4, active3).map(_.id()) ++ activeOrdersByPair
    val allOrdersByPairWithTwoNew    = List(active4, active3).map(_.id()) ++ allOrdersByPair

    {
      val xs = activeOrders
      xs should matchTo(allOrders.take(xs.size)) // rest were finalized
      xs should matchTo(activeOrdersAllNine)
    }

    activeOrdersBy(alicePair) should matchTo(activeOrdersByPairWithTwoNew)
    allOrdersBy(alicePair) should matchTo(allOrdersByPairWithTwoNew)

    info("'orderHistoryByPair' must return no more 'rest-order-limit' orders")

    val active7  = mkOrder(alice, alicePair, SELL, 1, 9.waves, ts = now + 17)
    val active8  = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 18)
    val active9  = mkOrder(alice, bobPair, BUY, 1, 1.waves, ts = now + 19)
    val active10 = mkOrder(alice, bobPair, BUY, 1, 1.waves, ts = now + 20)
    List(active7, active8, active9, active10).foreach(dex1.api.place)

    dex1.api.waitForOrderStatus(active10, Status.Accepted)

    val activeOrdersAllThirteen               = List(active10, active9, active8, active7).map(_.id()) ++ activeOrdersAllNine
    val activeOrdersByPairWithTwoMoreNew      = List(active8, active7).map(_.id()) ++ activeOrdersByPairWithTwoNew
    val allOrdersByPairWithTwoNewExceptOneOld = List(active8, active7).map(_.id()) ++ allOrdersByPairWithTwoNew

    {
      val xs = activeOrders
      xs should matchTo(allOrders.take(xs.size)) // rest were finalized
      xs should matchTo(activeOrdersAllThirteen)
    }

    activeOrdersBy(alicePair) should matchTo(activeOrdersByPairWithTwoMoreNew)
    allOrdersBy(alicePair) should matchTo(allOrdersByPairWithTwoNewExceptOneOld)

    info("all the methods move active orders that were filled")

    List(
      mkOrder(bob, bobPair, SELL, 1, 3.waves, ts = now + 21), // fill partial2
      mkOrder(bob, bobPair, SELL, 2, 2.waves, ts = now + 22), // fill active2, active5
      mkOrder(bob, alicePair, BUY, 2, 9.waves, ts = now + 23), // fill partial1, active7
      mkOrder(bob, alicePair, BUY, 1, 10.waves, ts = now + 24) // fill active1
    ).foreach(placeAndAwaitAtDex(_, Status.Filled))

    val activeOrdersAllSeven = List(active10, active9, active8, active6, active4, active3, active0).map(_.id())

    // Because sorted by timestamp in descending order
    val allOrdersWithOneFilled          = activeOrdersAllSeven ++ Seq(active7, partial1, active1).map(_.id())
    val activeOrdersByPairWithTwoFilled = List(active8, active4, active3, active0).map(_.id())
    val allOrdersByPairWithTwoFilled    = activeOrdersByPairWithTwoFilled ++ Seq(active7, partial1, active1).map(_.id())

    activeOrders should matchTo(activeOrdersAllSeven)
    allOrders should matchTo(allOrdersWithOneFilled)
    activeOrdersBy(alicePair) should matchTo(activeOrdersByPairWithTwoFilled)
    allOrdersBy(alicePair) should matchTo(allOrdersByPairWithTwoFilled)

    info("'orderHistoryByPair' must return all active orders, even if they are more than the limit")

    val active11 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 25)
    val active12 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 26)
    val active13 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 27)
    val active14 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 28)
    val active15 = mkOrder(alice, alicePair, SELL, 1, 10.waves, ts = now + 29)
    List(active11, active12, active13, active14, active15).foreach(dex1.api.place)

    dex1.api.waitForOrderStatus(active15, Status.Accepted)

    val activeOrdersAllTwelve     = List(active15, active14, active13, active12, active11).map(_.id()) ++ activeOrdersAllSeven
    val activeOrdersByPairAllNine = List(active15, active14, active13, active12, active11).map(_.id()) ++ allOrdersByPairWithTwoFilled

    {
      val xs = activeOrders
      xs should matchTo(allOrders.take(xs.size))
      xs should matchTo(activeOrdersAllTwelve.take(xs.size))
    }

    {
      val xs = activeOrdersBy(alicePair)
      xs should matchTo(allOrdersBy(alicePair).take(xs.size)) // rest were finalized
      xs should matchTo(activeOrdersByPairAllNine.take(xs.size))
    }
  }

}
