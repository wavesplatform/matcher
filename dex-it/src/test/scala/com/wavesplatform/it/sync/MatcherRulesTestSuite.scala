package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class MatcherRulesTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val matcherRulesStr =
      s"""
         |waves.dex {
         |  matching-rules = {
         |    "$WctId-$UsdId": [
         |      {
         |        start-offset = 2
         |        tick-size    = 5
         |      },
         |      {
         |        start-offset = 12
         |        tick-size    = 7
         |      },
         |      {
         |        start-offset = 13
         |        tick-size    = 10
         |      },
         |      {
         |        start-offset = 15
         |        tick-size    = 8
         |      },
         |      {
         |        start-offset = 17
         |        tick-size    = 9
         |      }
         |    ]
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(matcherRulesStr).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_)).foreach { tx =>
      node.waitForTransaction(tx.id)
    }
  }

  val (amount, price) = (1000L, PriceConstant)

  // offset +6
  "Orders should be cancelled correctly when matcher rules are changed" in {

    // here tick size is disabled (offset = 0)
    val buyOrder1 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder1, "Accepted")

    // here tick size is disabled (offset = 1)
    val buyOrder2 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder2, "Accepted")

    // here tick size = 5 (offset = 2), hence new order is placed into corrected price level 5, not 7
    val buyOrder3 = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder3, "Accepted")

    // now there are 2 price levels
    node.orderBook(wctUsdPair).bids.map(_.price) shouldBe Seq(7 * price, 5 * price)

    // price level 5 will be deleted after cancelling of buyOrder3
    node.cancelOrder(alice, wctUsdPair, buyOrder3)
    node.waitOrderStatus(wctUsdPair, buyOrder3, "Cancelled")

    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(2 * amount, 7 * price))
    Array(buyOrder1, buyOrder2).foreach(order => node.cancelOrder(alice, wctUsdPair, order))
  }

  // offset +4
  "matching on the same price level" in {
    val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance
    val bobWctBalance = node.assetBalance(bob.address, WctId.toString).balance
    val aliceUsdBalance = node.assetBalance(alice.address, UsdId.toString).balance
    val aliceWctBalance = node.assetBalance(alice.address, WctId.toString).balance
    val bobWavesBalance = node.accountBalances(bob.address)._1
    val aliceWavesBalance = node.accountBalances(alice.address)._1

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 3 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, sellOrder, "Accepted")
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))

    val anotherSellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 4 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, anotherSellOrder, "Accepted")

    val buyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder, "Filled")
    node.waitOrderInBlockchain(buyOrder)

    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))
    node.orderBook(wctUsdPair).bids shouldBe Nil

    node.assertAssetBalance(bob.address, UsdId.toString, bobUsdBalance + 3 * amount)
    node.assertAssetBalance(bob.address, WctId.toString, bobWctBalance - amount)
    node.assertAssetBalance(alice.address, UsdId.toString, aliceUsdBalance - 3 * amount)
    node.assertAssetBalance(alice.address, WctId.toString, aliceWctBalance + amount)
    node.assertBalances(bob.address, bobWavesBalance - matcherFee)
    node.assertBalances(alice.address, aliceWavesBalance - matcherFee)

    node.cancelOrder(bob, wctUsdPair, anotherSellOrder)
  }

  // offset +4
  "start offset with matched orders" in {
    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 3 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))
    node.placeOrder(alice, wctUsdPair, BUY, amount, 3 * price, matcherFee).message.id
    node.waitOrderInBlockchain(sellOrder)

    val buyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, 10 * price, matcherFee).message.id
    node.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 7 * price))
    node.cancelOrder(alice, wctUsdPair, buyOrder)
  }

  // offset +2
  "cancellation after tick size changed" in {
    val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance
    val bobWctBalance = node.assetBalance(bob.address, WctId.toString).balance

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 9 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, sellOrder, "Accepted")
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 10 * price))
    node.reservedBalance(bob)("WAVES") shouldBe 300000
    node.reservedBalance(bob)(WctId.toString) shouldBe amount

    node.cancelOrder(bob, wctUsdPair, sellOrder)
    node.assertAssetBalance(bob.address, UsdId.toString, bobUsdBalance)
    node.assertAssetBalance(bob.address, WctId.toString, bobWctBalance)
    node.reservedBalance(bob) shouldBe Map()
  }

  "matching with old orders after tick size enabled" in {
    val bobUsdBalance = node.assetBalance(bob.address, UsdId.toString).balance
    val bobWctBalance = node.assetBalance(bob.address, WctId.toString).balance
    val aliceUsdBalance = node.assetBalance(alice.address, UsdId.toString).balance
    val aliceWctBalance = node.assetBalance(alice.address, WctId.toString).balance
    val bobWavesBalance = node.accountBalances(bob.address)._1
    val aliceWavesBalance = node.accountBalances(alice.address)._1

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 9 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, sellOrder, "Accepted")
    node.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 16 * price))

    val buyOrder = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, 20 * price, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder, "Filled")
    node.waitOrderInBlockchain(buyOrder)

    node.assertAssetBalance(bob.address, UsdId.toString, bobUsdBalance + 9 * amount)
    node.assertAssetBalance(bob.address, WctId.toString, bobWctBalance - amount)
    node.assertAssetBalance(alice.address, UsdId.toString, aliceUsdBalance - 9 * amount)
    node.assertAssetBalance(alice.address, WctId.toString, aliceWctBalance + amount)
    node.assertBalances(bob.address, bobWavesBalance - matcherFee)
    node.assertBalances(alice.address, aliceWavesBalance - matcherFee / 2)
  }
}
