package com.wavesplatform.it.sync.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig.IssueUsdTx
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}


class GetOrderBookTestSuite extends MatcherSuiteBase{

  override protected def nodeConfigs: Seq[Config] = {

    val orderFeeSettingsStr =
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

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val txIds = Seq(IssueUsdTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))
  }

  "response orderbook should contain right count of bids and asks" in {
    for (i <- 1 to 50) {
      node.placeOrder(alice, wavesUsdPair, BUY, 1.waves, i, 300000, version = 3)
    }
    for (i <- 51 to 101) {
      node.placeOrder(alice, wavesUsdPair, SELL, 1.waves, i, 300000, version = 3)
    }

    val orderBookDepth10 = node.orderBook(wavesUsdPair, 10)
    orderBookDepth10.asks.size shouldBe 10
    orderBookDepth10.bids.size shouldBe 10
    Array(0, 1, 8, 9).foreach(depth => {
        node.orderBook(wavesUsdPair, depth) shouldBe orderBookDepth10
      })

    val anotherOrderBookDepth = node.orderBook(wavesUsdPair, 20)
    anotherOrderBookDepth.asks.size shouldBe 20
    anotherOrderBookDepth.bids.size shouldBe 20
    Array(11, 12, 19).foreach(depth => {
        node.orderBook(wavesUsdPair, depth) shouldBe anotherOrderBookDepth
      })

    val orderBookDepth40 = node.orderBook(wavesUsdPair, 40)
    orderBookDepth40.asks.size shouldBe 40
    orderBookDepth40.bids.size shouldBe 40

    val orderBookDepth41 = node.orderBook(wavesUsdPair, 41)
    orderBookDepth41.asks.size shouldBe 41
    orderBookDepth41.bids.size shouldBe 41

    val orderBookDepth43 = node.orderBook(wavesUsdPair, 43)
    node.orderBook(wavesUsdPair, 42) shouldBe orderBookDepth43
    orderBookDepth43.asks.size + orderBookDepth43.bids.size shouldBe 86

    val defaultOrderBook = node.orderBook(wavesUsdPair)
    defaultOrderBook shouldBe node.orderBook(wavesUsdPair, 100)
    Array(44, 45, 60, 98, 99).foreach(depth => {
      node.orderBook(wavesUsdPair, depth) shouldBe defaultOrderBook
    })

    val hundredAndOneOrderBookDepth = node.orderBook(wavesUsdPair, 101)
    Array(102, 103, 999, 9999).foreach(depth => {
        node.orderBook(wavesUsdPair, depth) shouldBe hundredAndOneOrderBookDepth
      })
  }
}
