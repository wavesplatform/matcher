package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.{MatcherError, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

class MatcherTickerTestSuite extends NewMatcherSuiteBase {
  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = ["$UsdId", "WAVES"]""".stripMargin)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcast(IssueUsdTx, IssueBtcTx)
  }

  private val btcUsdPair = AssetPair(
    amountAsset = btc,
    priceAsset = usd
  )

  // Because BTC is not a price asset in this test
  private val btcWavesPair = AssetPair(
    amountAsset = btc,
    priceAsset = Waves
  )

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      dex1Api.tryOrderBookStatus(wctUsdPair) should failWith(11534345, MatcherError.Params(assetId = Some(WctId.toString)))
    }

    "status of empty orderbook" in {
//    TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(dex1Api.orderBookStatus(wavesUsdPair), s"")
    }

    "error of non-existed order" in {
      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.orderStatus(IssueUsdTx.id().toString, wavesUsdPair), s"")
    }

    "try to work with incorrect pair" in {
      val usdWavesPair = AssetPair(usd, Waves)

      intercept[RuntimeException] {
        dex1Api.tryOrderBook(usdWavesPair)
      }

      dex1Api.tryOrderBook(wavesUsdPair) shouldBe 'right
//      assert(
//        node
//          .matcherGet(s"/matcher/orderbook/${usdWavesPair.amountAssetStr}/${usdWavesPair.priceAssetStr}/status", statusCode = 301)
//          .getHeader("Location")
//          .contains(s"WAVES/${usdWavesPair.amountAssetStr}"))

      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(dex1Api.place(mkOrder(node,usdWavesPair, OrderType.BUY, 1.waves, 200), ""))
    }

    val bidPrice  = 200
    val bidAmount = 1.waves
    val askPrice  = 400
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      dex1Api.place(mkOrder(alice, btcUsdPair, OrderType.BUY, bidAmount, bidPrice))

      val aliceOrder = mkOrder(alice, btcUsdPair, OrderType.BUY, bidAmount, bidPrice)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Accepted)

      val r = dex1Api.orderBookStatus(btcUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe None
      r.askAmount shouldBe None
    }

    "place ask order for second pair" in {
      dex1Api.place(mkOrder(bob, btcWavesPair, OrderType.SELL, askAmount, askPrice))

      val bobOrder = mkOrder(bob, btcWavesPair, OrderType.SELL, askAmount, askPrice)
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)

      val r = dex1Api.orderBookStatus(btcWavesPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "place ask order for first pair" in {
      dex1Api.place(mkOrder(bob, btcUsdPair, OrderType.SELL, askAmount, askPrice))

      val bobOrder = mkOrder(bob, btcUsdPair, OrderType.SELL, askAmount, askPrice)
      dex1Api.place(bobOrder)
      dex1Api.waitForOrderStatus(bobOrder, OrderStatus.Accepted)

      val r = dex1Api.orderBookStatus(btcUsdPair)
      r.lastPrice shouldBe None
      r.lastSide shouldBe None
      r.bid shouldBe Some(bidPrice)
      r.bidAmount shouldBe Some(2 * bidAmount)
      r.ask shouldBe Some(askPrice)
      r.askAmount shouldBe Some(2 * askAmount)
    }

    "match bid order for first pair" in {
      val bobOrder1 = mkOrder(bob, btcUsdPair, OrderType.SELL, askAmount, bidPrice)
      dex1Api.place(bobOrder1)
      dex1Api.waitForOrderStatus(bobOrder1, OrderStatus.Filled)

      val r1 = dex1Api.orderBookStatus(btcUsdPair)
      r1.lastPrice shouldBe Some(bidPrice)
      r1.lastSide shouldBe Some("sell") // TODO stringly typed
      r1.bid shouldBe Some(bidPrice)
      r1.bidAmount shouldBe Some(2 * bidAmount - askAmount)
      r1.ask shouldBe Some(askPrice)
      r1.askAmount shouldBe Some(2 * askAmount)

      val bobOrder2 = mkOrder(bob, btcUsdPair, OrderType.SELL, 3 * askAmount, bidPrice)
      dex1Api.place(bobOrder2).message.id
      dex1Api.waitForOrderStatus(bobOrder2, OrderStatus.Filled)

      val r2 = dex1Api.orderBookStatus(btcUsdPair)
      r2.lastPrice shouldBe Some(bidPrice)
      r2.lastSide shouldBe Some("sell")
      r2.bid shouldBe None
      r2.bidAmount shouldBe None
      r2.ask shouldBe Some(askPrice)
      r2.askAmount shouldBe Some(2 * askAmount)
    }

    "match ask order for first pair" in {
      val aliceOrder = mkOrder(alice, btcUsdPair, OrderType.BUY, bidAmount, askPrice)
      dex1Api.place(aliceOrder)
      dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

      val r = dex1Api.orderBookStatus(btcUsdPair)
      r.lastPrice shouldBe Some(askPrice)
      r.lastSide shouldBe Some("buy")
      r.bid shouldBe None
      r.bidAmount shouldBe None
      r.ask shouldBe None
      r.askAmount shouldBe None
    }
  }

}
