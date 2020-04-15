package com.wavesplatform.it.sync.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.responses.dex.OrderBookResponse
import com.wavesplatform.it.MatcherSuiteBase

class GetOrderBookTestSuite extends MatcherSuiteBase {

  val ordersCount = 0

  override protected val dexInitialSuiteConfig: Config =
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
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  def checkDepth(depth: Int, array: Array[Int] = Array()): Unit = {
    val orderBook = clear(dex1.api.orderBook(wavesUsdPair, depth))

    if (depth < ordersCount) {
      orderBook.asks.size shouldBe depth
      orderBook.bids.size shouldBe depth
    }

    array.foreach(depth => clear(dex1.api.orderBook(wavesUsdPair, depth)) should matchTo(orderBook))
  }

  "response order book should contain right count of bids and asks" in {

    for (i <- 1 to ordersCount) {
      dex1.api.place(mkOrder(alice, wavesUsdPair, BUY, 1.waves, i, 300000, version = 3))
      dex1.api.place(mkOrder(alice, wavesUsdPair, SELL, 1.waves, i + 51, 300000, version = 3))
    }

    checkDepth(10, Array(0, 1, 8, 9))
    checkDepth(20, Array(11, 12, 19))
    checkDepth(50)
    checkDepth(101, Array(102, 103, 999, 9999))

    withClue("check default depth value") {
      val defaultOrderBook = clear(dex1.api.orderBook(wavesUsdPair))
      defaultOrderBook should matchTo(clear(dex1.api.orderBook(wavesUsdPair, 100)))
      Array(44, 45, 60, 98, 99).foreach(depth => clear(dex1.api.orderBook(wavesUsdPair, depth)) shouldBe clear(defaultOrderBook))
    }
  }

  private def clear(x: OrderBookResponse) = x.copy(timestamp = 0L) // DEX-642
}
