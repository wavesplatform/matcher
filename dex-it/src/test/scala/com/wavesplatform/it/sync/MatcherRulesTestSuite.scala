package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.{LevelResponse, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class MatcherRulesTestSuite extends NewMatcherSuiteBase {
  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(s"""
       |waves.dex.matching-rules.$wctUsdPair = [
       |  {
       |    start-offset = 2
       |    merge-prices = yes
       |    tick-size    = 5
       |  }
       |]
       """.stripMargin)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueUsdTx, IssueWctTx)
  }

  private val (amount, price) = (1000L, PriceConstant)

  "Orders should be cancelled correctly when matcher rules are changed" in {
    // here tick size is disabled (offset = 0)
    val buyOrder1 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price)
    dex1Api.place(buyOrder1)
    dex1Api.waitForOrderStatus(buyOrder1, OrderStatus.Accepted)

    // here tick size is disabled (offset = 1)
    val buyOrder2 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price)
    dex1Api.place(buyOrder2)
    dex1Api.waitForOrderStatus(buyOrder2, OrderStatus.Accepted)

    // here tick size = 5 (offset = 2), hence new order is placed into corrected price level 5, not 7
    val buyOrder3 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price)
    dex1Api.place(buyOrder3)
    dex1Api.waitForOrderStatus(buyOrder3, OrderStatus.Accepted)

    // now there are 2 price levels
    dex1Api.orderBook(wctUsdPair).bids.map(_.price) shouldBe Seq(7 * price, 5 * price)

    // price level 5 will be deleted after cancelling of buyOrder3
    dex1Api.cancel(alice, buyOrder3)
    dex1Api.waitForOrderStatus(buyOrder3, OrderStatus.Cancelled)

    dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(2 * amount, 7 * price))
  }
}
