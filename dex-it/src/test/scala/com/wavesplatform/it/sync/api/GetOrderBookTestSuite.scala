package com.wavesplatform.it.sync.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class GetOrderBookTestSuite extends MatcherSuiteBase {

  val ordersCount = 0

  override protected val suiteInitialDexConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  allowed-order-versions = [1, 2, 3]
         |  order-book-snapshot-http-cache {
         |    cache-timeout = 5s
         |    depth-ranges = [10, 20, 40, 41, 43, 100, 1000]
         |    default-depth = 100
         |  }
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueUsdTx)
    startAndWait(dex1Container(), dex1Api)
  }

  def checkDepth(depth: Int, array: Array[Int] = Array()): Unit = {
    val orderBook = dex1Api.orderBook(wavesUsdPair, depth)

    if (depth < ordersCount) {
      orderBook.asks.size shouldBe depth
      orderBook.bids.size shouldBe depth
    }

    array.foreach(depth => dex1Api.orderBook(wavesUsdPair, depth) shouldBe orderBook)
  }

  "response order book should contain right count of bids and asks" in {

    for (i <- 1 to ordersCount) {
      dex1Api.place(mkOrder(alice, wavesUsdPair, BUY, 1.waves, i, 300000, version = 3))
      dex1Api.place(mkOrder(alice, wavesUsdPair, SELL, 1.waves, i + 51, 300000, version = 3))
    }

    checkDepth(10, Array(0, 1, 8, 9))
    checkDepth(20, Array(11, 12, 19))
    checkDepth(50)
    checkDepth(101, Array(102, 103, 999, 9999))

    withClue("check default depth value") {
      val defaultOrderBook = dex1Api.orderBook(wavesUsdPair)
      defaultOrderBook shouldBe dex1Api.orderBook(wavesUsdPair, 100)
      Array(44, 45, 60, 98, 99).foreach(depth => dex1Api.orderBook(wavesUsdPair, depth) shouldBe defaultOrderBook)
    }
  }
}
