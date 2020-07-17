package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.responses.dex.MatcherError
import com.wavesplatform.dex.model.{LastTrade, LevelAgg}
import com.wavesplatform.it.MatcherSuiteBase

class MatcherTickerTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = ["$UsdId", "WAVES"]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  // Because BTC is not a price asset in this test
  private val btcWavesPair = AssetPair(
    amountAsset = btc,
    priceAsset = Waves
  )

  "matcher ticker validation" - {
    "get tickers for unavailable asset should produce error" in {
      dex1.api.tryOrderBookStatus(wctUsdPair) should failWith(11534345, MatcherError.Params(assetId = Some(WctId.toString)))
    }

    "status of empty orderbook" in {
//    TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(dex1.api.orderBookStatus(wavesUsdPair), s"")
    }

    "error of non-existed order" in {
      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(node.orderStatus(IssueUsdTx.id().toString, wavesUsdPair), s"")
    }

    "try to work with incorrect pair" in {
      val usdWavesPair = AssetPair(usd, Waves)

      intercept[RuntimeException] {
        dex1.api.tryOrderBook(usdWavesPair)
      }

      dex1.api.tryOrderBook(wavesUsdPair) shouldBe Symbol("right")
//      assert(
//        node
//          .matcherGet(s"/matcher/orderbook/${usdWavesPair.amountAssetStr}/${usdWavesPair.priceAssetStr}/status", statusCode = 301)
//          .getHeader("Location")
//          .contains(s"WAVES/${usdWavesPair.amountAssetStr}"))

      //TODO: add error message after fix of https://wavesplatform.atlassian.net/browse/NODE-1151
//      SyncMatcherHttpApi.assertNotFoundAndMessage(dex1.api.place(mkOrder(node,usdWavesPair, OrderType.BUY, 1.waves, 200), ""))
    }

    val bidPrice  = 200
    val bidAmount = 1.waves
    val askPrice  = 400
    val askAmount = bidAmount / 2

    "place bid order for first pair" in {
      dex1.api.place(mkOrder(alice, btcUsdPair, OrderType.BUY, bidAmount, bidPrice))
      placeAndAwaitAtDex(mkOrder(alice, btcUsdPair, OrderType.BUY, bidAmount, bidPrice))

      val r = dex1.api.orderBookStatus(btcUsdPair)
      r.lastTrade shouldBe None
      r.bestBid should matchTo { Option(LevelAgg(2 * bidAmount, bidPrice)) }
      r.bestAsk shouldBe None
    }

    "place ask order for second pair" in {
      dex1.api.place(mkOrder(bob, btcWavesPair, OrderType.SELL, askAmount, askPrice))
      placeAndAwaitAtDex(mkOrder(bob, btcWavesPair, OrderType.SELL, askAmount, askPrice))

      val r = dex1.api.orderBookStatus(btcWavesPair)
      r.lastTrade shouldBe None
      r.bestBid shouldBe None
      r.bestAsk should matchTo { Option(LevelAgg(2 * askAmount, askPrice)) }
    }

    "place ask order for first pair" in {
      dex1.api.place(mkOrder(bob, btcUsdPair, OrderType.SELL, askAmount, askPrice))
      placeAndAwaitAtDex(mkOrder(bob, btcUsdPair, OrderType.SELL, askAmount, askPrice))

      val r = dex1.api.orderBookStatus(btcUsdPair)
      r.lastTrade shouldBe None
      r.bestBid should matchTo { Option(LevelAgg(2 * bidAmount, bidPrice)) }
      r.bestAsk should matchTo { Option(LevelAgg(2 * askAmount, askPrice)) }
    }

    "match bid order for first pair" in {
      placeAndAwaitAtDex(mkOrder(bob, btcUsdPair, OrderType.SELL, askAmount, bidPrice), Status.Filled)

      val r1 = dex1.api.orderBookStatus(btcUsdPair)
      r1.lastTrade should matchTo { Option(LastTrade(bidPrice, askAmount, OrderType.SELL)) }
      r1.bestBid should matchTo { Option(LevelAgg(2 * bidAmount - askAmount, bidPrice)) }
      r1.bestAsk should matchTo { Option(LevelAgg(2 * askAmount, askPrice)) }

      placeAndAwaitAtDex(mkOrder(bob, btcUsdPair, OrderType.SELL, 3 * askAmount, bidPrice), Status.Filled)

      val r2 = dex1.api.orderBookStatus(btcUsdPair)
      r2.lastTrade should matchTo { Option(LastTrade(bidPrice, 2 * askAmount, OrderType.SELL)) } // second BUY order (bidAmount = 2 * askAmount) filled
      r2.bestBid shouldBe None
      r2.bestAsk should matchTo { Option(LevelAgg(2 * askAmount, askPrice)) }
    }

    "match ask order for first pair" in {
      placeAndAwaitAtDex(mkOrder(alice, btcUsdPair, OrderType.BUY, bidAmount, askPrice), Status.Filled)

      val r = dex1.api.orderBookStatus(btcUsdPair)
      r.lastTrade should matchTo { Option(LastTrade(askPrice, askAmount, OrderType.BUY)) } // second SELL order filled
      r.bestBid shouldBe None
      r.bestAsk shouldBe None
    }
  }

}
