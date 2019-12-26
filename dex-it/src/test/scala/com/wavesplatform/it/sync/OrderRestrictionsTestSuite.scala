package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

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

  "order should be rejected with correct code and message when price is more then Long volume" ignore {
//    val tooHighPrice = "10000000000000000000"
//    dex1.api.tryPlace(
//      mkOrder(alice, wavesUsdPair, SELL, 1000000000L, 1000000000000000000L).json.value() ++ Json.obj("price" -> tooHighPrice),
//      "The provided JSON contains invalid fields: /price. Check the documentation"
//    )
  }

  "order should be rejected with correct code and message when amount is more then Long volume" ignore {
//    val tooLargeAmount = "10000000000000000000"
//    dex1.api.tryPlace(
//      mkOrder(alice, wavesUsdPair, SELL, 1000000000L, 1000000L).json.value() ++ Json.obj("amount" -> tooLargeAmount),
//      "The provided JSON contains invalid fields: /amount. Check the documentation"
//    )
  }

  "order info returns information event there is no such order book" in {
    dex1.api.orderBookInfo(ethBtcPair).restrictions shouldBe empty
    dex1.api.orderBookInfo(wavesBtcPair).restrictions shouldBe empty
    dex1.api.orderBookInfo(wctUsdPair).restrictions shouldNot be(empty)
  }

  "low amount" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 1, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 0.01 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "high amount" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 98778997800000123L, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 987789978000001.23 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "wrong step amount" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 15, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 0.15 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "low price" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000, 25, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 0.00000025 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "high price" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000, 1000000000000L, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 10000 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "wrong step price" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000, 150000, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 0.0015 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "invalid both amount & price" in {
    dex1.api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000000L, 150000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 1000000000 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "valid order" in {
    placeAndAwait(mkOrder(alice, wctUsdPair, BUY, 100000000, 100000, matcherFee))
  }

  "order restrictions endpoints" in {
    dex1.api.orderBookInfo(wctUsdPair).restrictions.get.minAmount shouldBe "0.1"
    dex1.api.orderBookInfo(wctUsdPair).restrictions.get.maxAmount shouldBe "100000000"
    dex1.api.orderBookInfo(wctUsdPair).restrictions.get.stepAmount shouldBe "0.1"
    dex1.api.orderBookInfo(wctUsdPair).restrictions.get.minPrice shouldBe "0.0001"
    dex1.api.orderBookInfo(wctUsdPair).restrictions.get.maxPrice shouldBe "1000"
    dex1.api.orderBookInfo(wctUsdPair).restrictions.get.stepPrice shouldBe "0.001"

    dex1.api.tradingPairInfo(wctUsdPair).get.restrictions.get.minAmount shouldBe "0.1"
    dex1.api.tradingPairInfo(wctUsdPair).get.restrictions.get.maxAmount shouldBe "100000000"
    dex1.api.tradingPairInfo(wctUsdPair).get.restrictions.get.stepAmount shouldBe "0.1"
    dex1.api.tradingPairInfo(wctUsdPair).get.restrictions.get.minPrice shouldBe "0.0001"
    dex1.api.tradingPairInfo(wctUsdPair).get.restrictions.get.maxPrice shouldBe "1000"
    dex1.api.tradingPairInfo(wctUsdPair).get.restrictions.get.stepPrice shouldBe "0.001"

    dex1.api.orderBookInfo(wavesBtcPair).restrictions shouldBe empty

    dex1.api.place(mkOrder(bob, wavesBtcPair, BUY, 100000000, 100000, matcherFee))
    dex1.api.tradingPairInfo(wavesBtcPair).get.restrictions shouldBe empty
  }
}
