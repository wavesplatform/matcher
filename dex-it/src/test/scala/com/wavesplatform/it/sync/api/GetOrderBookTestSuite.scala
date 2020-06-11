package com.wavesplatform.it.sync.api

import com.softwaremill.diffx.{Derived, Diff}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.ApiV0OrderBook
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.MatcherSuiteBase

class GetOrderBookTestSuite extends MatcherSuiteBase {

  private val ordersCount = 150

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES", $EthId ]
         |  order-book-http {
         |    depth-ranges = [10, 20, 40, 41, 43, 100, 1000]
         |    default-depth = 100
         |  }
         |}""".stripMargin
    )

  // DEX-642
  private implicit val orderBookResponseDiff: Diff[ApiV0OrderBook] = Derived[Diff[ApiV0OrderBook]].ignore(_.timestamp)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueEthTx)
    dex1.start()
  }

  def checkDepth(forTheseDepths: Array[Int] = Array(), thisDepthWillBePicked: Int): Unit = {
    val orderBook: ApiV0OrderBook = dex1.api.orderBook(wavesUsdPair, thisDepthWillBePicked)

    if (thisDepthWillBePicked < ordersCount) {
      orderBook.asks.size shouldBe thisDepthWillBePicked
      orderBook.bids.size shouldBe thisDepthWillBePicked
    }

    forTheseDepths.foreach(depth => dex1.api.orderBook(wavesUsdPair, depth) should matchTo(orderBook))
  }

  "response order book should contain right count of bids and asks" in {

    for (i <- 1 to ordersCount) {
      dex1.api.place(mkOrder(alice, wavesUsdPair, BUY, 1.waves, i))
      dex1.api.place(mkOrder(alice, wavesUsdPair, SELL, 1.waves, i + ordersCount + 1))
    }

    checkDepth(forTheseDepths = Array(0, 1, 8, 9), thisDepthWillBePicked = 10)
    checkDepth(forTheseDepths = Array(11, 12, 19), thisDepthWillBePicked = 20)
    checkDepth(forTheseDepths = Array(31, 32, 39), thisDepthWillBePicked = 40)
    checkDepth(forTheseDepths = Array(42), thisDepthWillBePicked = 43)
    checkDepth(forTheseDepths = Array(102, 103, 999, 9999), thisDepthWillBePicked = 1000)

    withClue("check default depth value") {
      val defaultOrderBook = dex1.api.orderBook(wavesUsdPair)
      defaultOrderBook should matchTo(dex1.api.orderBook(wavesUsdPair, 100))
      Array(44, 45, 60, 98, 99).foreach(depth => dex1.api.orderBook(wavesUsdPair, depth) should matchTo(defaultOrderBook))
    }
  }

  "query parameters should not be lost during redirect to well-ordered pair" in {

    val depth               = 10
    val ethWavesOrdersCount = 20

    (1 to ethWavesOrdersCount) foreach { i =>
      dex1.api.place(mkOrder(alice, ethWavesPair, BUY, 1.waves, i * 100))
      dex1.api.place(mkOrder(alice, ethWavesPair, SELL, 1.waves, (i + ethWavesOrdersCount + 1) * 100))
    }

    val orderBook = dex1.api.orderBook(AssetPair(Waves, eth), depth)
    orderBook.asks should have size depth
    orderBook.bids should have size depth
  }
}
