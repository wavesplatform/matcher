package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus}
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class MatcherRulesTestSuite extends MatcherSuiteBase {
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
    broadcastAndAwait(IssueUsdTx, IssueWctTx)
  }

  private val (amount, price) = (1000L, PriceConstant)

  "Orders should be cancelled correctly when matcher rules are changed" in {
    // here tick size is disabled (offset = 0)
    val buyOrder1 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price)
    placeAndAwait(buyOrder1)

    // here tick size is disabled (offset = 1)
    val buyOrder2 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price)
    placeAndAwait(buyOrder2)

    // here tick size = 5 (offset = 2), hence new order is placed into corrected price level 5, not 7
    val buyOrder3 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price)
    placeAndAwait(buyOrder3)

    // now there are 2 price levels
    dex1Api.orderBook(wctUsdPair).bids.map(_.price) shouldBe Seq(7 * price, 5 * price)

    // price level 5 will be deleted after cancelling of buyOrder3
    dex1Api.cancel(alice, buyOrder3)
    dex1Api.waitForOrderStatus(buyOrder3, OrderStatus.Cancelled)

    dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(2 * amount, 7 * price))
  }
}
