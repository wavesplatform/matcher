package com.wavesplatform.it.sync.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class GetOrderBookTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config =
    ConfigFactory.parseString(
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-book-snapshot-http-cache {
         |    cache-timeout = 5s
         |    depth-ranges = [10, 20, 40, 41, 43, 100, 1000]
         |    default-depth = 100
         |  }
         |}
       """.stripMargin
    )

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
  }

  "response order book should contain right count of bids and asks" in {

    (1 to 50).foreach { i =>
      dex1Api.place(mkOrder(alice, wavesUsdPair, BUY, 1.waves, i, 300000))
    }

    (51 to 101).foreach { i =>
      dex1Api.place(mkOrder(alice, wavesUsdPair, SELL, 1.waves, i, 300000))
    }

    val orderBookDepth10 = dex1Api.orderBook(wavesUsdPair, 10)
    orderBookDepth10.asks.size shouldBe 10
    orderBookDepth10.bids.size shouldBe 10
    Array(0, 1, 8, 9).foreach(depth => dex1Api.orderBook(wavesUsdPair, depth) shouldBe orderBookDepth10)

    val anotherOrderBookDepth = dex1Api.orderBook(wavesUsdPair, 20)
    anotherOrderBookDepth.asks.size shouldBe 20
    anotherOrderBookDepth.bids.size shouldBe 20
    Array(11, 12, 19).foreach(depth => dex1Api.orderBook(wavesUsdPair, depth) shouldBe anotherOrderBookDepth)

    val orderBookDepth40 = dex1Api.orderBook(wavesUsdPair, 40)
    orderBookDepth40.asks.size shouldBe 40
    orderBookDepth40.bids.size shouldBe 40

    val orderBookDepth41 = dex1Api.orderBook(wavesUsdPair, 41)
    orderBookDepth41.asks.size shouldBe 41
    orderBookDepth41.bids.size shouldBe 41

    val orderBookDepth43 = dex1Api.orderBook(wavesUsdPair, 43)
    dex1Api.orderBook(wavesUsdPair, 42) shouldBe orderBookDepth43
    orderBookDepth43.asks.size + orderBookDepth43.bids.size shouldBe 86

    val defaultOrderBook = dex1Api.orderBook(wavesUsdPair)
    defaultOrderBook shouldBe dex1Api.orderBook(wavesUsdPair, 100)
    Array(44, 45, 60, 98, 99).foreach(depth => dex1Api.orderBook(wavesUsdPair, depth) shouldBe defaultOrderBook)

    val hundredAndOneOrderBookDepth = dex1Api.orderBook(wavesUsdPair, 101)
    Array(102, 103, 999, 9999).foreach(depth => dex1Api.orderBook(wavesUsdPair, depth) shouldBe hundredAndOneOrderBookDepth)
  }
}
