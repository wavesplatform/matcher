package com.wavesplatform.it.sync

import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.it.MatcherSuiteBase

class TradingMarketsTestSuite extends MatcherSuiteBase {
  val (amount, price) = (1000L, 1000000000L)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueWctTx)
  }

  "When some orders were placed and matcher was restarted" - {
    "Trading markets have info about all asset pairs" in {
      placeAndAwaitAtDex(mkOrder(alice, wctWavesPair, BUY, amount, price))

      dex1.restart()

      val markets = dex1.api.allOrderBooks.markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}
