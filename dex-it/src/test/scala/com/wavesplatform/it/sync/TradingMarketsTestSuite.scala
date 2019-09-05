package com.wavesplatform.it.sync

import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class TradingMarketsTestSuite extends NewMatcherSuiteBase {
  val (amount, price) = (1000L, 1000000000L)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueWctTx)
  }

  "When some orders were placed and matcher was restarted" - {
    "Trading markets have info about all asset pairs" in {
      val order = mkOrder(alice, wctWavesPair, BUY, amount, price)

      // TODO utility method for combination
      dex1Api.place(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Accepted)

      restartContainer(dex1Container(), dex1Api)

      val markets = dex1Api.allOrderBooks.markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}
