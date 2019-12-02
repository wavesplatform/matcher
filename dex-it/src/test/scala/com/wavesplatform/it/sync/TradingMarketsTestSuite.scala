package com.wavesplatform.it.sync

import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class TradingMarketsTestSuite extends MatcherSuiteBase {
  val (amount, price) = (1000L, 1000000000L)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueWctTx)
  }

  "When some orders were placed and matcher was restarted" - {
    "Trading markets have info about all asset pairs" in {
      placeAndAwait(mkOrder(alice, wctWavesPair, BUY, amount, price))

      restartContainer(dex1Container(), dex1Api)

      val markets = dex1Api.allOrderBooks.markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}
