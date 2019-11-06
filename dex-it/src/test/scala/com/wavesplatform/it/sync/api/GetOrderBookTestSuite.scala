package com.wavesplatform.it.sync.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig.IssueUsdTx
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.it.util._

class GetOrderBookTestSuite extends MatcherSuiteBase {
  val ordersCount = 0

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
    node.cancelOrdersForPair(alice, wavesUsdPair)
  }

  def checkDepth(depth: Int, array: Array[Int] = Array()): Unit = {
    val orderBook = node.orderBook(wavesUsdPair, depth)

    if (depth < ordersCount) {
      orderBook.asks.size shouldBe depth
      orderBook.bids.size shouldBe depth
    }

    array.foreach(depth => {
      node.orderBook(wavesUsdPair, depth) shouldBe orderBook
    })
  }

  "response orderbook should contain right count of bids and asks" in {
    for (i <- 1 to ordersCount) {
      node.waitOrderStatus(wavesUsdPair, node.placeOrder(alice, wavesUsdPair, BUY, 1.waves, i, 300000, version = 3).message.id, "Accepted")
      node.waitOrderStatus(wavesUsdPair, node.placeOrder(alice, wavesUsdPair, SELL, 1.waves, i + 51, 300000, version = 3).message.id, "Accepted")
    }

    checkDepth(10, Array(0, 1, 8, 9))
    checkDepth(20, Array(11, 12, 19))
    checkDepth(50)
    checkDepth(101, Array(102, 103, 999, 9999))

    withClue("check default depth value") {
      val defaultOrderBook = node.orderBook(wavesUsdPair)
      defaultOrderBook shouldBe node.orderBook(wavesUsdPair, 100)
      Array(44, 45, 60, 98, 99).foreach(depth => {
        node.orderBook(wavesUsdPair, depth) shouldBe defaultOrderBook
      })
    }
  }
}
