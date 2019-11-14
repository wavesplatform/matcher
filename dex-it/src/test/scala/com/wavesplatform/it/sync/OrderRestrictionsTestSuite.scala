package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType.BUY

class OrderRestrictionsTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config =
    ConfigFactory.parseString(
      s"""
         |waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
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
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueBtcTx)
    startAndWait(dex1Container(), dex1Api)
  }

  "low amount" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 1, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 0.01 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "high amount" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 98778997800000123L, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 987789978000001.23 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "wrong step amount" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 15, 100000000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 0.15 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "low price" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000, 25, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 0.00000025 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "high price" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000, 1000000000000L, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 10000 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "wrong step price" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000, 150000, matcherFee)) should failWith(
      9441282, // OrderInvalidPrice
      "The order's price 0.0015 does not meet matcher's requirements: max price = 1000, min price = 0.0001, step price = 0.001"
    )
  }

  "invalid both amount & price" in {
    dex1Api.tryPlace(mkOrder(alice, wctUsdPair, BUY, 100000000000L, 150000, matcherFee)) should failWith(
      9441026, // OrderInvalidAmount
      s"The order's amount 1000000000 $WctId does not meet matcher's requirements: max amount = 100000000, min amount = 0.1, step amount = 0.1"
    )
  }

  "valid order" in {
    placeAndAwait(mkOrder(alice, wctUsdPair, BUY, 100000000, 100000, matcherFee))
  }

  "order restrictions endpoints" in {
    dex1Api.orderBookInfo(wctUsdPair).restrictions.get.minAmount shouldBe "0.1"
    dex1Api.orderBookInfo(wctUsdPair).restrictions.get.maxAmount shouldBe "100000000"
    dex1Api.orderBookInfo(wctUsdPair).restrictions.get.stepAmount shouldBe "0.1"
    dex1Api.orderBookInfo(wctUsdPair).restrictions.get.minPrice shouldBe "0.0001"
    dex1Api.orderBookInfo(wctUsdPair).restrictions.get.maxPrice shouldBe "1000"
    dex1Api.orderBookInfo(wctUsdPair).restrictions.get.stepPrice shouldBe "0.001"

    dex1Api.tradingPairInfo(wctUsdPair).get.restrictions.get.minAmount shouldBe "0.1"
    dex1Api.tradingPairInfo(wctUsdPair).get.restrictions.get.maxAmount shouldBe "100000000"
    dex1Api.tradingPairInfo(wctUsdPair).get.restrictions.get.stepAmount shouldBe "0.1"
    dex1Api.tradingPairInfo(wctUsdPair).get.restrictions.get.minPrice shouldBe "0.0001"
    dex1Api.tradingPairInfo(wctUsdPair).get.restrictions.get.maxPrice shouldBe "1000"
    dex1Api.tradingPairInfo(wctUsdPair).get.restrictions.get.stepPrice shouldBe "0.001"

    dex1Api.orderBookInfo(wavesBtcPair).restrictions.isEmpty

    dex1Api.place(mkOrder(bob, wavesBtcPair, BUY, 100000000, 100000, matcherFee))
    dex1Api.tradingPairInfo(wavesBtcPair).get.restrictions.isEmpty
  }
}
