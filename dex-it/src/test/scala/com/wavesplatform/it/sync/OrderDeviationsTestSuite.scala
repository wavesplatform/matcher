package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus}
//import com.wavesplatform.it.util.DoubleExt
//import com.wavesplatform.protobuf.order.Order.Side.SELL
//import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

/**
  * BUY orders:  (1 - p) * best bid <= price <= (1 + l) * best ask
  * SELL orders: (1 - l) * best bid <= price <= (1 + p) * best ask
  *
  * where:
  *
  *   p = max price deviation profit / 100
  *   l = max price deviation loss / 100
  *   best bid = highest price of buy
  *   best ask = lowest price of sell
  */
class OrderDeviationsTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    """waves.dex {
      |  max-price-deviations {
      |    enable = yes
      |    profit = 70
      |    loss = 60
      |    fee = 50
      |  }
      |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueBtcTx)
  }

  "buy orders price is" - {
    "in deviation bounds" in {
      /*val bestAskOrder = mkOrder(alice, wavesBtcPair, SELL, 1000.waves, 500000)
      placeAndAwait(bestAskOrder)
      dex1Api.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

      val bestBidOrder = mkOrder(bob, wavesBtcPair, BUY, 1000.waves, 400000)
      dex1Api.place(bestBidOrder)*/
      pending
    }

    "out of deviation bounds" - {
      "-- too low" in {
        pending
      }

      "-- too high" in {
        pending
      }
    }
  }

  "sell orders price is" - {
    "in deviation bounds" in {
      pending
    }

    "out of deviation bounds" - {
      "-- too low" in {
        pending
      }

      "-- too high" in {
        pending
      }
    }
  }

  "orders fee is" - {
    "in deviation bounds" in {
      pending
    }

    "out of deviation bounds" in {
      pending
    }
  }
}
