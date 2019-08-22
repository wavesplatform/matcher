package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

/**
  * BUY orders price:  (1 - p) * best bid <= price <= (1 + l) * best ask
  * SELL orders price: (1 - l) * best bid <= price <= (1 + p) * best ask
  *
  * where:
  *
  *   p = max price deviation profit / 100
  *   l = max price deviation loss / 100
  *   best bid = highest price of buy
  *   best ask = lowest price of sell
  *
  * BUY orders fee:  fee >= fs * (1 - fd) * best ask * amount
  * SELL orders fee: fee >= fs * (1 - fd) * best bid * amount
  *
  * where:
  *
  *   fs = fee in percents from order-fee settings (order-fee.percent.min-fee) / 100
  *   fd = max fee deviation / 100
  *   best bid = highest price of buy
  *   best ask = lowest price of sell
  */
class OrderDeviationsTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {
    val orderDeviations =
      s"""
         |waves.dex {
         |  max-price-deviations {
         |    enable = yes
         |    profit = 70
         |    loss = 60
         |    fee = 40
         |  }
         |
         |  order-fee {
         |    mode = "percent"
         |
         |    percent {
         |      asset-type = "price"
         |      min-fee = 0.1
         |    }
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(orderDeviations).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    node.waitForTransaction(node.broadcastRequest(IssueBtcTx.json()).id)
  }

  "buy orders price is" - {
    "in deviation bounds" in {
      val bestAskOrderId  = node.placeOrder(alice, wavesBtcPair, SELL, 2000.waves, 500000, matcherFee).message.id
      val bestBidOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 2000.waves, 300000, 2 * matcherFee).message.id
      Array(bestAskOrderId, bestBidOrderId).foreach(orderId =>
        node.waitOrderStatus(wavesBtcPair, orderId, expectedStatus = "Accepted"))
      node.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(2000.waves, 500000))
      node.orderBook(wavesBtcPair).bids shouldBe List(LevelResponse(2000.waves, 300000))

      Array(90000 -> "Accepted", 800000 -> "Filled").foreach { case (price, status) =>
        node.waitOrderStatus(wavesBtcPair, node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, price, 2 * matcherFee).message.id, expectedStatus = status)
      }
      Array(alice, bob).foreach(sender => node.cancelAllOrders(sender))
    }

    "out of deviation bounds" - {
      "-- too low" in {
        val bestBidOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 300000, matcherFee).message.id
        node.waitOrderStatus(wavesBtcPair, bestBidOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesBtcPair).bids shouldBe List(LevelResponse(1000.waves, 300000))

        assertBadRequestAndMessage(node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 89999, matcherFee),
          "The order's price 0.00089999 is out of deviation bounds (max profit: 70% and max loss: 60% in relation to the best bid/ask)", 400)
        node.cancelOrder(bob, wavesBtcPair, bestBidOrderId)
      }

      "-- too high" in {
        val bestAskOrderId  = node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 500000, matcherFee).message.id
        node.waitOrderStatus(wavesBtcPair, bestAskOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

        assertBadRequestAndMessage(node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 800001, matcherFee),
          "The order's price 0.00800001 is out of deviation bounds (max profit: 70% and max loss: 60% in relation to the best bid/ask)", 400)
        node.cancelOrder(alice, wavesBtcPair, bestAskOrderId)
      }
    }
  }

  "sell orders price is" - {
    "in deviation bounds" in {
      val bestAskOrderId  = node.placeOrder(alice, wavesBtcPair, SELL, 2000.waves, 500000, matcherFee).message.id
      val bestBidOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 2000.waves, 300000, 2 * matcherFee).message.id
      Array(bestAskOrderId, bestBidOrderId).foreach(orderId =>
        node.waitOrderStatus(wavesBtcPair, orderId, expectedStatus = "Accepted"))
      node.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(2000.waves, 500000))
      node.orderBook(wavesBtcPair).bids shouldBe List(LevelResponse(2000.waves, 300000))

      Array(850000 -> "Accepted", 120000 -> "Filled").foreach { case (price, status) =>
        node.waitOrderStatus(wavesBtcPair, node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, price, 2 * matcherFee).message.id, expectedStatus = status)
      }
      Array(alice, bob).foreach(sender => node.cancelAllOrders(sender))
    }

    "out of deviation bounds" - {
      "-- too low" in {
        val bestBidOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 300000, matcherFee).message.id
        node.waitOrderStatus(wavesBtcPair, bestBidOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesBtcPair).bids shouldBe List(LevelResponse(1000.waves, 300000))

        assertBadRequestAndMessage(node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 119999, matcherFee),
          "The order's price 0.00119999 is out of deviation bounds (max profit: 70% and max loss: 60% in relation to the best bid/ask)", 400)
        node.cancelOrder(bob, wavesBtcPair, bestBidOrderId)
      }

      "-- too high" in {
        val bestAskOrderId = node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 500000, matcherFee).message.id
        node.waitOrderStatus(wavesBtcPair, bestAskOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

        assertBadRequestAndMessage(node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 850001, matcherFee),
          "The order's price 0.00850001 is out of deviation bounds (max profit: 70% and max loss: 60% in relation to the best bid/ask)", 400)
        node.cancelOrder(alice, wavesBtcPair, bestAskOrderId)
      }
    }
  }

  "orders fee is" - {
    "in deviation bounds" in {
      val bestAskOrderId = node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 600000, matcherFee).message.id
      node.waitOrderStatus(wavesBtcPair, bestAskOrderId, expectedStatus = "Accepted")
      node.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(1000.waves, 600000))

      val bobOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 800000, 360000).message.id
      node.waitOrderStatus(wavesBtcPair, bobOrderId, expectedStatus = "Filled")

      val bestBidOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 700000, matcherFee).message.id
      node.waitOrderStatus(wavesBtcPair, bestBidOrderId, expectedStatus = "Accepted")
      node.orderBook(wavesBtcPair).bids shouldBe List(LevelResponse(1000.waves, 700000))

      val aliceOrderId = node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 600000, 420000).message.id
      node.waitOrderStatus(wavesBtcPair, aliceOrderId, expectedStatus = "Filled")
    }

    "out of deviation bounds" in {
      val bestAskOrderId = node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 600000, matcherFee).message.id
      node.waitOrderStatus(wavesBtcPair, bestAskOrderId, expectedStatus = "Accepted")
      node.orderBook(wavesBtcPair).asks shouldBe List(LevelResponse(1000.waves, 600000))

      assertBadRequestAndMessage(node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 800000, 359999).message.id,
        "The order's matcher fee 0.00359999 WAVES is out of deviation bounds (max deviation: 40% in relation to the best bid/ask)", 400)
      node.cancelOrder(alice, wavesBtcPair, bestAskOrderId)

      val bestBidOrderId = node.placeOrder(bob, wavesBtcPair, BUY, 1000.waves, 700000, matcherFee).message.id
      node.waitOrderStatus(wavesBtcPair, bestBidOrderId, expectedStatus = "Accepted")
      node.orderBook(wavesBtcPair).bids shouldBe List(LevelResponse(1000.waves, 700000))

      assertBadRequestAndMessage(node.placeOrder(alice, wavesBtcPair, SELL, 1000.waves, 600000, 419999).message.id,
        "The order's matcher fee 0.00419999 WAVES is out of deviation bounds (max deviation: 40% in relation to the best bid/ask)", 400)
      node.cancelOrder(bob, wavesBtcPair, bestBidOrderId)
    }
  }
}
