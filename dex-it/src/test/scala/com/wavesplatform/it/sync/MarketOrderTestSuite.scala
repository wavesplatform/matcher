package com.wavesplatform.it.sync

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

class MarketOrderTestSuite extends NewMatcherSuiteBase {

  override protected def dex1Config: Config =
    ConfigFactory
      .parseString("waves.dex.allowed-order-versions = [1, 2, 3]")
      .withFallback(super.dex1Config)

  val (amount, price) = (1000L, PriceConstant)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    issueAssets(IssueUsdTx, IssueWctTx, IssueEthTx)
    List(UsdAsset, WctAsset, EthAsset).foreach(asset => dex1Api.upsertRate(asset, 1.0)._1 shouldBe StatusCodes.Created)
  }

  "Sunny day tests for market orders" in {

    // Alice has WAVES, ETH, USD => Alice can buy WCT @ WAVES (fee in ETH) and sell ETH @ WAVES (fee in USD)
    // Bob has WAVES, WCT => Bob can sell WCT @ WAVES and buy ETH @ WAVES

    def bigBuyOrder: Order   = prepareOrder(alice, matcher, wctUsdPair, BUY, 10 * amount, price, matcherFeeAssetId = EthAsset)
    def smallBuyOrder: Order = prepareOrder(alice, matcher, wctUsdPair, BUY, 5 * amount, price, matcherFeeAssetId = EthAsset)

    def bigSellOrder: Order   = prepareOrder(alice, matcher, ethWavesPair, SELL, 10 * amount, price, matcherFeeAssetId = UsdAsset)
    def smallSellOrder: Order = prepareOrder(alice, matcher, ethWavesPair, SELL, 5 * amount, price, matcherFeeAssetId = UsdAsset)

    def bestPrice(counterOrderType: OrderType): Double = counterOrderType match { case SELL => price * 0.97; case _ => price * 1.03 }
    def goodPrice(counterOrderType: OrderType): Double = counterOrderType match { case SELL => price * 0.98; case _ => price * 1.02 }

    def placeCounterOrdersOfType(counterOrderType: OrderType, pair: AssetPair): Unit = {
      val orders = List(
        prepareOrder(bob, matcher, pair, counterOrderType, 3 * amount, bestPrice(counterOrderType).toLong),
        prepareOrder(bob, matcher, pair, counterOrderType, 2 * amount, goodPrice(counterOrderType).toLong),
        prepareOrder(bob, matcher, pair, counterOrderType, 1 * amount, goodPrice(counterOrderType).toLong)
      )
      orders.foreach(dex1Api.place)
      orders.foreach(dex1Api.waitForOrderStatus(_, OrderStatus.Accepted))
    }

    withClue("Big buy market order executed partially:") {
      placeCounterOrdersOfType(SELL, wctUsdPair)
      val order = bigBuyOrder
      dex1Api.placeMarket(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Filled).filledAmount shouldBe Some(6 * amount) //

      dex1Api.reservedBalance(alice) shouldBe empty
      dex1Api.orderBook(wctUsdPair).bids shouldBe empty
      dex1Api.orderBook(wctUsdPair).asks shouldBe empty
    }

    withClue("Small buy market order executed fully:") {
      placeCounterOrdersOfType(SELL, wctUsdPair)
      val order = smallBuyOrder
      dex1Api.placeMarket(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Filled).filledAmount shouldBe Some(5 * amount)

      dex1Api.reservedBalance(alice) shouldBe empty
      dex1Api.orderBook(wctUsdPair).bids shouldBe empty
      dex1Api.orderBook(wctUsdPair).asks.size shouldBe 1
      dex1Api.orderBook(wctUsdPair).asks.head.amount shouldBe 1 * amount
      dex1Api.orderBook(wctUsdPair).asks.head.price shouldBe goodPrice(SELL)
    }

    dex1Api.cancelAll(bob)

    withClue("Big sell market order executed partially:") {
      placeCounterOrdersOfType(BUY, ethWavesPair)
      val order = bigSellOrder
      dex1Api.placeMarket(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Filled).filledAmount shouldBe Some(6 * amount)

      dex1Api.reservedBalance(alice) shouldBe empty
      dex1Api.orderBook(ethWavesPair).asks shouldBe empty
      dex1Api.orderBook(ethWavesPair).bids shouldBe empty
    }

    withClue("Small sell market order executed fully:") {
      placeCounterOrdersOfType(BUY, ethWavesPair)
      val order = smallSellOrder
      dex1Api.placeMarket(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Filled).filledAmount shouldBe Some(5 * amount)

      dex1Api.reservedBalance(alice) shouldBe empty
      dex1Api.orderBook(ethWavesPair).asks shouldBe empty
      dex1Api.orderBook(ethWavesPair).bids.size shouldBe 1
      dex1Api.orderBook(ethWavesPair).bids.head.amount shouldBe 1 * amount
      dex1Api.orderBook(ethWavesPair).bids.head.price shouldBe goodPrice(BUY)
    }
  }

  "Market order should be executed correctly when available for spending < required by spendable asset" in {
    pending
  }
}
