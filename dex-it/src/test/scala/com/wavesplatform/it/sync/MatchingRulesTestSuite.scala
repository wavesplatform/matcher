package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig.{wctUsdPair, _}
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.AssetPair

class MatchingRulesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val matcherRulesStr =
      s"""
         |waves.dex {
         |  matching-rules = {
         |    "$WctId-$UsdId": [
         |      {
         |        start-offset = 8
         |        tick-size    = 8
         |      },
         |      {
         |        start-offset = 9
         |        tick-size    = 7
         |      },
         |      {
         |        start-offset = 12
         |        tick-size    = 0.01
         |      },
         |      {
         |        start-offset = 14
         |        tick-size    = 5
         |      },
         |      {
         |        start-offset = 22
         |        tick-size    = 10
         |      },
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ],
         |    "$WctId-WAVES": [
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ],
         |    "WAVES-$BtcId": [
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ],
         |    "WAVES-$UsdId": [
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ]
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(matcherRulesStr).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueWctTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_)).foreach { tx =>
      node.waitForTransaction(tx.id)
    }
  }

  def priceAssetBalance(owner: KeyPair, assetPair: AssetPair): Long = {
    assetBalance(owner, assetPair.priceAssetStr)
  }

  def amountAssetBalance(owner: KeyPair, assetPair: AssetPair): Long = {
    assetBalance(owner, assetPair.amountAssetStr)
  }

  def assetBalance(owner: KeyPair, assetId: String): Long = {
    if (assetId == "WAVES")
      node.accountBalances(owner.toAddress.toString)._1
    else {
      node.assetBalance(owner.toAddress.toString, assetId).balance
    }
  }

  val (amount, price) = (1000L, PriceConstant)

  // offset is 0, after test - 6
  "Tick size isn't set" in {
    node.tradingMarkets().markets.size shouldBe 0
    // TODO check orderbookInfo for wct-Waves
    Array(wctUsdPair -> "0.00000001", wavesBtcPair -> "0.00000001", wavesUsdPair -> "0.01").foreach { case (pair, defaultTs) =>
      // TODO uncomment after "DEX-421 Internal server error in OrderbookInfo request" will be solved
      // node.orderbookInfo(pair).matchingRules.tickSize shouldBe defaultTs

      val orderId = node.placeOrder(bob, pair, SELL, amount, price, matcherFee).message.id
      node.waitOrderStatus(pair, orderId, "Accepted")
      node.tradingPairInfo(pair).get.matchingRules.tickSize shouldBe defaultTs
      node.cancelOrder(bob, pair, orderId)
    }
  }

  // offset is 6, after test - 12
  "Start offset depends of order placing and cancelling but matching" in {
    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 15 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 15 * price))
    node.placeOrder(alice, wctUsdPair, BUY, amount, 17 * price, matcherFee).message.id
    node.getCurrentOffset shouldBe 7

    node.waitOrderInBlockchain(sellOrder)

    node.getCurrentOffset shouldBe 7

    val buyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, 10 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 8 * price))
    node.cancelOrder(alice, wctUsdPair, buyOrder)
    node.getCurrentOffset shouldBe 9

    val anotherBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, 10 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 7 * price))
    node.cancelOrder(alice, wctUsdPair, anotherBuyOrder)
  }

  // offset is 12, after test - 18
  "Orders should be cancelled correctly when matcher rules are changed" in {

    // here tick size is disabled (offset = 12)
    val buyOrder1 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder1, "Accepted")

    // here tick size is disabled (offset = 13)
    val buyOrder2 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder2, "Accepted")

    val aliceUsdBalance = node.assetBalance(alice.toAddress.toString, UsdId.toString).balance
    val aliceWctBalance = node.assetBalance(alice.toAddress.toString, WctId.toString).balance

    // here tick size = 5 (offset = 14), hence new order is placed into corrected price level 5, not 7
    val buyOrder3 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder3, "Accepted")

    // now there are 2 price levels
    node.orderBook(wctUsdPair).bids.map(_.price) shouldBe Seq(7 * price, 5 * price)

    node.reservedBalance(alice)("WAVES") shouldBe matcherFee * 3
    node.reservedBalance(alice)(UsdId.toString) shouldBe amount * 7 * 3 * price / PriceConstant

    // price level 5 will be deleted after cancelling of buyOrder3
    node.cancelOrder(alice, wctUsdPair, buyOrder3)
    node.waitOrderStatus(wctUsdPair, buyOrder3, "Cancelled")

    node.assertAssetBalance(alice.toAddress.toString, UsdId.toString, aliceUsdBalance)
    node.assertAssetBalance(alice.toAddress.toString, WctId.toString, aliceWctBalance)
    node.reservedBalance(alice)("WAVES") shouldBe matcherFee * 2
    node.reservedBalance(alice)(UsdId.toString) shouldBe amount * 2 * 7 * price / PriceConstant

    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(2 * amount, 7 * price))
    Array(buyOrder1, buyOrder2).foreach(order => node.cancelOrder(alice, wctUsdPair, order))
  }

  // offset is 18, after test - 22
  "Matching on the same price level" in {
    val bobUsdBalance = node.assetBalance(bob.toAddress.toString, UsdId.toString).balance
    val bobWctBalance = node.assetBalance(bob.toAddress.toString, WctId.toString).balance
    val aliceUsdBalance = node.assetBalance(alice.toAddress.toString, UsdId.toString).balance
    val aliceWctBalance = node.assetBalance(alice.toAddress.toString, WctId.toString).balance
    val bobWavesBalance = node.accountBalances(bob.toAddress.toString)._1
    val aliceWavesBalance = node.accountBalances(alice.toAddress.toString)._1

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 4 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, sellOrder, "Accepted")
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))

    val anotherSellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 3 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, anotherSellOrder, "Accepted")

    val buyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder, "Filled")
    node.waitOrderInBlockchain(buyOrder)

    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))
    node.orderBook(wctUsdPair).bids shouldBe Nil

    node.assertAssetBalance(bob.toAddress.toString, UsdId.toString, bobUsdBalance + 4 * amount)
    node.assertAssetBalance(bob.toAddress.toString, WctId.toString, bobWctBalance - amount)
    node.assertAssetBalance(alice.toAddress.toString, UsdId.toString, aliceUsdBalance - 4 * amount)
    node.assertAssetBalance(alice.toAddress.toString, WctId.toString, aliceWctBalance + amount)
    node.assertBalances(bob.toAddress.toString, bobWavesBalance - matcherFee)
    node.assertBalances(alice.toAddress.toString, aliceWavesBalance - matcherFee)

    node.cancelOrder(bob, wctUsdPair, anotherSellOrder)
  }

  // offset is 22, after test - 25
  "Matching with old orders after tick size enabled" in {
    val bobUsdBalance = node.assetBalance(bob.toAddress.toString, UsdId.toString).balance
    val bobWctBalance = node.assetBalance(bob.toAddress.toString, WctId.toString).balance
    val aliceUsdBalance = node.assetBalance(alice.toAddress.toString, UsdId.toString).balance
    val aliceWctBalance = node.assetBalance(alice.toAddress.toString, WctId.toString).balance
    val bobWavesBalance = node.accountBalances(bob.toAddress.toString)._1
    val aliceWavesBalance = node.accountBalances(alice.toAddress.toString)._1

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 11 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, sellOrder, "Accepted")
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 20 * price))

    val buyOrder = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, 25 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder, "Filled")
    node.waitOrderInBlockchain(buyOrder)

    node.assertAssetBalance(bob.toAddress.toString, UsdId.toString, bobUsdBalance + 11 * amount)
    node.assertAssetBalance(bob.toAddress.toString, WctId.toString, bobWctBalance - amount)
    node.assertAssetBalance(alice.toAddress.toString, UsdId.toString, aliceUsdBalance - 11 * amount)
    node.assertAssetBalance(alice.toAddress.toString, WctId.toString, aliceWctBalance + amount)
    node.assertBalances(bob.toAddress.toString, bobWavesBalance - matcherFee)
    node.assertBalances(alice.toAddress.toString, aliceWavesBalance - matcherFee / 2)

    withClue("partially filled order cancellation") {
      node.reservedBalance(alice)("WAVES") shouldBe matcherFee / 2
      node.reservedBalance(alice)(UsdId.toString) shouldBe 25 * price * amount / PriceConstant
      node.cancelOrder(alice, wctUsdPair, buyOrder)
      node.reservedBalance(alice) shouldBe Map()
    }
  }

  // offset is 25, after test - 29
  "Matching orders of same price but neighbors levels" in {
    val bestAskOrderId = node.placeOrder(alice, wctUsdPair, SELL, amount, 17 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 24 * price))
    val bestBidOrderId = node.placeOrder(bob, wctUsdPair, BUY, amount, 17 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 12 * price))

    node.cancelOrder(alice, wctUsdPair, bestAskOrderId)
    node.cancelOrder(bob, wctUsdPair, bestBidOrderId)
  }

  "Placing order on level 0" in {
    assertBadRequestAndMessage(node.placeOrder(bob, wctUsdPair, BUY, amount * 100000000L, 1, matcherFee),
      "The buy order's price 0.00000001 does not meet matcher's requirements: price >= 12 (actual tick size). Orders can not be placed into level with price 0")
  }

  "Placing order on level 0 with virgin orderbook" in {
    // TODO remove pending after DEX-404 will be solved
    pending
    assertBadRequestAndMessage(node.placeOrder(bob, wctWavesPair, BUY, amount * 100000000L, 1 * 1000000L, matcherFee),
      "The buy order's price 0.00000001 does not meet matcher's requirements: price >= 12 (actual tick size). Orders can not be placed into level with price 0")
  }

  "Matching orders with different decimals" in {
    Array((wctUsdPair, amount, price), (wctWavesPair, amount, price * 1000000L), (wavesUsdPair, 1.waves, 100L), (wavesBtcPair, amount, price)).foreach {
      case (pair: AssetPair, amount: Long, price: Long) =>
        withClue(pair) {
          val aliceAmountBalance = assetBalance(alice, pair.amountAssetStr)
          val bobAmountBalance = assetBalance(bob, pair.amountAssetStr)
          val alicePriceBalance = assetBalance(alice, pair.priceAssetStr)
          val bobPriceBalance = assetBalance(bob, pair.priceAssetStr)
          val aliceWavesBalance = assetBalance(alice, "WAVES")
          val bobWavesBalance = assetBalance(bob, "WAVES")

          val bestAskOrderId = node.placeOrder(alice, pair, SELL, amount, 17 * price, matcherFee).message.id
          node.orderBook(pair).asks shouldBe Seq(LevelResponse(amount, 24 * price))
          val bestBidOrderId = node.placeOrder(bob, pair, BUY, amount, 17 * price, matcherFee).message.id
          node.orderBook(pair).bids shouldBe Seq(LevelResponse(amount, 12 * price))

          node.cancelOrder(alice, pair, bestAskOrderId)
          node.cancelOrder(bob, pair, bestBidOrderId)

          val filledOrderId = node.placeOrder(bob, pair, BUY, amount, 25 * price, matcherFee).message.id
          val partiallyFilledOrderId = node.placeOrder(alice, pair, SELL, 2 * amount, 17 * price, matcherFee).message.id
          node.waitOrderStatus(pair, filledOrderId, expectedStatus = "Filled")
          node.waitOrderStatus(pair, partiallyFilledOrderId, expectedStatus = "PartiallyFilled")
          node.waitOrderInBlockchain(filledOrderId)

          pair match {
            case `wctUsdPair` =>
              assetBalance(alice, pair.amountAssetStr) shouldBe aliceAmountBalance - amount
              assetBalance(bob, pair.amountAssetStr) shouldBe bobAmountBalance + amount
              assetBalance(alice, pair.priceAssetStr) shouldBe alicePriceBalance + 25000L
              assetBalance(bob, pair.priceAssetStr) shouldBe bobPriceBalance - 25000L
              assetBalance(alice, "WAVES") shouldBe aliceWavesBalance - matcherFee / 2
              assetBalance(bob, "WAVES") shouldBe bobWavesBalance - matcherFee
            case `wctWavesPair` =>
              assetBalance(alice, pair.amountAssetStr) shouldBe aliceAmountBalance - amount
              assetBalance(bob, pair.amountAssetStr) shouldBe bobAmountBalance + amount
              assetBalance(alice, pair.priceAssetStr) shouldBe alicePriceBalance + 25000000000L - matcherFee / 2
              assetBalance(bob, pair.priceAssetStr) shouldBe bobPriceBalance - 25000000000L - matcherFee
            case `wavesUsdPair` =>
              assetBalance(alice, pair.amountAssetStr) shouldBe aliceAmountBalance - amount - matcherFee / 2
              assetBalance(bob, pair.amountAssetStr) shouldBe bobAmountBalance + amount - matcherFee
              assetBalance(alice, pair.priceAssetStr) shouldBe alicePriceBalance + 2500L
              assetBalance(bob, pair.priceAssetStr) shouldBe bobPriceBalance - 2500L
            case `wavesBtcPair` =>
              assetBalance(alice, pair.amountAssetStr) shouldBe aliceAmountBalance - amount - matcherFee / 2
              assetBalance(bob, pair.amountAssetStr) shouldBe bobAmountBalance + amount - matcherFee
              assetBalance(alice, pair.priceAssetStr) shouldBe alicePriceBalance + 25000L
              assetBalance(bob, pair.priceAssetStr) shouldBe bobPriceBalance - 25000L
            case _ => ???
          }

          node.cancelOrder(alice, pair, partiallyFilledOrderId)
        }
    }
  }
}
