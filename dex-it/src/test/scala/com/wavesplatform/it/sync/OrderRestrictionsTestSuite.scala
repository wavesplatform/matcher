package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class OrderRestrictionsTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val orderRestrictionsStr =
      s"""
         |waves.dex {
         |  order-restrictions = {
         |   "$WctId-$UsdId": {
         |     min-amount  = 0.1
         |     max-amount  = 100000000
         |     step-amount = 0.1
         |     min-price   = 0.0001
         |     max-price   = 1000
         |     step-price  = 0.001
         |   }
         | }
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(orderRestrictionsStr).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_)).foreach { tx =>
      node.waitForTransaction(tx.id)
    }
  }

  "low amount order" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 1, 100000000, matcherFee),
      s"The order's amount 0.01 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1")
  }

  "high amount order" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 100000000000L, 100000000, matcherFee),
      s"The order's amount 1000000000 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1")
  }

  "wrong fraction amount" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 15, 100000000, matcherFee),
      s"The order's amount 0.15 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1")
  }

  "low price amount" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 100000000, 25, matcherFee),
      "The order's price 0.00000025 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001")
  }

  "high price amount" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 100000000, 1000000000000L, matcherFee),
      "The order's price 10000 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001")
  }

  "wrong fraction price" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 100000000, 150000, matcherFee),
      "The order's price 0.0015 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001")
  }

  "invalid both amount & price" in {
    assertBadRequestAndResponse(node.placeOrder(alice, wctUsdPair, BUY, 100000000000L, 150000, matcherFee),
    s"The order's amount 1000000000 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1")
  }

  "valid order" in {
    val order = node.placeOrder(alice, wctUsdPair, BUY, 100000000, 100000, matcherFee).message.id
    node.waitOrderStatus(wctUsdPair, order, "Accepted")
  }

}
