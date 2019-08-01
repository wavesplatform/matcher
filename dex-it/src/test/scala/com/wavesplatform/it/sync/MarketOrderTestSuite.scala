package com.wavesplatform.it.sync

import akka.http.scaladsl.model.StatusCodes.Created
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

class MarketOrderTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = {

    val matcherRulesStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(matcherRulesStr).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    Seq(IssueUsdTx, IssueWctTx, IssueEthTx).map(_.json()).map(node.broadcastRequest(_)).foreach { tx =>
      node.waitForTransaction(tx.id)
    }

    node.upsertRate(ethAsset, 1.0, expectedStatusCode = Created)
    node.upsertRate(wctAsset, 1.0, expectedStatusCode = Created)
    node.upsertRate(usdAsset, 1.0, expectedStatusCode = Created)
  }

  val (amount, price) = (1000L, PriceConstant)
  val ethAsset        = IssuedAsset(EthId)
  val wctAsset        = IssuedAsset(WctId)
  val usdAsset        = IssuedAsset(UsdId)

  "Sunny day tests for market orders" in {

    // Alice has WAVES, ETH, USD => Alice can buy WCT @ WAVES (fee in ETH) and sell ETH @ WAVES (fee in USD)
    // Bob has WAVES, WCT => Bob can sell WCT @ WAVES and buy ETH @ WAVES

    def bigBuyOrder: Order   = node.prepareOrder(alice, wctUsdPair, BUY, 10 * amount, price, matcherFee, matcherFeeAssetId = ethAsset, version = 3)
    def smallBuyOrder: Order = node.prepareOrder(alice, wctUsdPair, BUY, 5 * amount, price, matcherFee, matcherFeeAssetId = ethAsset, version = 3)

    def bigSellOrder: Order   = node.prepareOrder(alice, ethWavesPair, SELL, 10 * amount, price, matcherFee, matcherFeeAssetId = usdAsset, version = 3)
    def smallSellOrder: Order = node.prepareOrder(alice, ethWavesPair, SELL, 5 * amount, price, matcherFee, matcherFeeAssetId = usdAsset, version = 3)

    def bestPrice(counterOrderType: OrderType): Double = counterOrderType match { case SELL => price * 0.97; case _ => price * 1.03 }
    def goodPrice(counterOrderType: OrderType): Double = counterOrderType match { case SELL => price * 0.98; case _ => price * 1.02 }

    def placeCounterOrdersOfType(counterOrderType: OrderType, pair: AssetPair): Unit = {
      Seq(
        node.placeOrder(bob, pair, counterOrderType, 3 * amount, bestPrice(counterOrderType).toLong, matcherFee).message.id,
        node.placeOrder(bob, pair, counterOrderType, 2 * amount, goodPrice(counterOrderType).toLong, matcherFee).message.id,
        node.placeOrder(bob, pair, counterOrderType, 1 * amount, goodPrice(counterOrderType).toLong, matcherFee).message.id
      ).foreach(lo => node.waitOrderStatus(pair, lo, "Accepted"))
    }

    withClue("Big buy market order executed partially:") {
      placeCounterOrdersOfType(SELL, wctUsdPair)
      node.waitOrderStatus(wctUsdPair, node.placeMarketOrder(bigBuyOrder).message.id, "Filled").filledAmount shouldBe Some(6 * amount)

      node.reservedBalance(alice) shouldBe empty
      node.orderBook(wctUsdPair).bids shouldBe empty
      node.orderBook(wctUsdPair).asks shouldBe empty
    }

    withClue("Small buy market order executed fully:") {
      placeCounterOrdersOfType(SELL, wctUsdPair)
      node.waitOrderStatus(wctUsdPair, node.placeMarketOrder(smallBuyOrder).message.id, "Filled").filledAmount shouldBe Some(5 * amount)

      node.reservedBalance(alice) shouldBe empty
      node.orderBook(wctUsdPair).bids shouldBe empty
      node.orderBook(wctUsdPair).asks.size shouldBe 1
      node.orderBook(wctUsdPair).asks.head.amount shouldBe 1 * amount
      node.orderBook(wctUsdPair).asks.head.price shouldBe goodPrice(SELL)
    }

    node.cancelAllOrders(bob)

    withClue("Big sell market order executed partially:") {
      placeCounterOrdersOfType(BUY, ethWavesPair)
      node.waitOrderStatus(ethWavesPair, node.placeMarketOrder(bigSellOrder).message.id, "Filled").filledAmount shouldBe Some(6 * amount)

      node.reservedBalance(alice) shouldBe empty
      node.orderBook(ethWavesPair).asks shouldBe empty
      node.orderBook(ethWavesPair).bids shouldBe empty
    }

    withClue("Small sell market order executed fully:") {
      placeCounterOrdersOfType(BUY, ethWavesPair)
      node.waitOrderStatus(ethWavesPair, node.placeMarketOrder(smallSellOrder).message.id, "Filled").filledAmount shouldBe Some(5 * amount)

      node.reservedBalance(alice) shouldBe empty
      node.orderBook(ethWavesPair).asks shouldBe empty
      node.orderBook(ethWavesPair).bids.size shouldBe 1
      node.orderBook(ethWavesPair).bids.head.amount shouldBe 1 * amount
      node.orderBook(ethWavesPair).bids.head.price shouldBe goodPrice(BUY)
    }
  }

  "Market order should be executed correctly when available for spending < required by spendable asset" in {
    pending
  }
}
