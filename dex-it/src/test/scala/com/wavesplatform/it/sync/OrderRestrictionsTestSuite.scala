package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.it.MatcherSuiteBase

class OrderRestrictionsTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""
         |waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES", "$EthId" ]
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
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueBtcTx, IssueEthTx)
    dex1.start()
  }

  // TODO DEX-547
  "order should be rejected with correct code and message when price is more then Long volume" ignore {
//    val tooHighPrice = "10000000000000000000"
//    dex1.tryApi.place(
//      mkOrder(alice, wavesUsdPair, SELL, 1000000000L, 1000000000000000000L).json.value() ++ Json.obj("price" -> tooHighPrice),
//      "The provided JSON contains invalid fields: /price. Check the documentation"
//    )
  }

  "order should be rejected with correct code and message when amount is more then Long volume" ignore {
//    val tooLargeAmount = "10000000000000000000"
//    dex1.tryApi.place(
//      mkOrder(alice, wavesUsdPair, SELL, 1000000000L, 1000000L).json.value() ++ Json.obj("amount" -> tooLargeAmount),
//      "The provided JSON contains invalid fields: /amount. Check the documentation"
//    )
  }

  "low amount" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 1, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 0.01 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "high amount" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 98778997800000123L, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 987789978000001.23 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "wrong step amount" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 15, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 0.15 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "low price" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 100000000, 25, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 0.00000025 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "high price" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 100000000, 1000000000000L, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 10000 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "wrong step price" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 100000000, 150000, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 0.0015 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "invalid both amount & price" in {
    dex1.tryApi.place(mkOrder(alice, wctUsdPair, BUY, 100000000000L, 150000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 1000000000 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "valid order" in {
    placeAndAwaitAtDex(mkOrder(alice, wctUsdPair, BUY, 100000000, 100000, matcherFee))
  }
}
