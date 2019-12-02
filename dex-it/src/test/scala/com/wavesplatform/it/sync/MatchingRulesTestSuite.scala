package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class MatchingRulesTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |  matching-rules = {
         |    "$WctId-$UsdId": [
         |      {
         |        start-offset = 8
         |        tick-size    = 8
         |      },
         |      {
         |        start-offset = 9
         |        tick-size    = 7
         |      },
         |      {
         |        start-offset = 12
         |        tick-size    = 0.01
         |      },
         |      {
         |        start-offset = 14
         |        tick-size    = 5
         |      },
         |      {
         |        start-offset = 22
         |        tick-size    = 10
         |      },
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ],
         |    "$WctId-WAVES": [
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ],
         |    "WAVES-$BtcId": [
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ],
         |    "WAVES-$UsdId": [
         |      {
         |        start-offset = 23
         |        tick-size    = 12
         |      }
         |    ]
         |  }
         |}
       """.stripMargin
    )

  override protected def beforeAll(): Unit = {
    // A custom initialization to guarantee that assets are in the blockchain
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueUsdTx, IssueWctTx, IssueBtcTx)
    startAndWait(dex1Container(), dex1Api)
  }

  def priceAssetBalance(owner: KeyPair, assetPair: AssetPair): Long = {
    assetBalance(owner, assetPair.priceAsset)
  }

  def amountAssetBalance(owner: KeyPair, assetPair: AssetPair): Long = {
    assetBalance(owner, assetPair.amountAsset)
  }

  def assetBalance(owner: KeyPair, asset: Asset): Long = {
    wavesNode1Api.balance(owner, asset)
  }

  val (amount, price) = (1000L, PriceConstant)

  // offset is 0, after test - 6
  "Tick size isn't set" in {
    dex1Api.allOrderBooks.markets.size shouldBe 0
    Seq(wctUsdPair -> "0.00000001", wavesBtcPair -> "0.00000001", wavesUsdPair -> "0.01").foreach {
      case (pair, defaultTs) =>
        dex1Api.orderBookInfo(pair).matchingRules.tickSize shouldBe defaultTs

        val order = mkOrder(bob, pair, SELL, amount, price, matcherFee)
        placeAndAwait(order)

        dex1Api.tradingPairInfo(pair).get.matchingRules.tickSize shouldBe defaultTs
        dex1Api.cancel(bob, order)
    }
  }

  // offset is 6, after test - 12
  "Start offset depends of order placing and cancelling but matching" in {
    val sellOrder = mkOrder(bob, wctUsdPair, SELL, amount, 15 * price, matcherFee)
    dex1Api.place(sellOrder)

    dex1Api.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 15 * price))
    dex1Api.place(mkOrder(alice, wctUsdPair, BUY, amount, 17 * price, matcherFee))
    dex1Api.waitForCurrentOffset(_ == 7)

    waitForOrderAtNode(sellOrder)

    val buyOrder = mkOrder(alice, wctUsdPair, BUY, amount, 10 * price, matcherFee)
    dex1Api.place(buyOrder)

    dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 8 * price))
    dex1Api.cancel(alice, buyOrder)
    dex1Api.waitForCurrentOffset(_ == 9)

    val anotherBuyOrder = mkOrder(alice, wctUsdPair, BUY, amount, 10 * price, matcherFee)
    dex1Api.place(anotherBuyOrder)

    dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 7 * price))
    dex1Api.cancel(alice, anotherBuyOrder)
  }

  // offset is 12, after test - 18
  "Orders should be cancelled correctly when matcher rules are changed" in {

    // here tick size is disabled (offset = 12)
    val buyOrder1 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee)
    placeAndAwait(buyOrder1)

    // here tick size is disabled (offset = 13)
    val buyOrder2 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee)
    placeAndAwait(buyOrder2)

    val aliceUsdBalance = wavesNode1Api.balance(alice, usd)
    val aliceWctBalance = wavesNode1Api.balance(alice, wct)

    // here tick size = 5 (offset = 14), hence new order is placed into corrected price level 5, not 7
    val buyOrder3 = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee)
    placeAndAwait(buyOrder3)

    // now there are 2 price levels
    dex1Api.orderBook(wctUsdPair).bids.map(_.price) shouldBe Seq(7 * price, 5 * price)

    dex1Api.reservedBalance(alice)(Waves) shouldBe matcherFee * 3
    dex1Api.reservedBalance(alice)(usd) shouldBe amount * 7 * 3 * price / PriceConstant

    // price level 5 will be deleted after cancelling of buyOrder3
    cancelAndAwait(alice, buyOrder3)

    wavesNode1Api.balance(alice, usd) shouldBe aliceUsdBalance
    wavesNode1Api.balance(alice, wct) shouldBe aliceWctBalance

    dex1Api.reservedBalance(alice)(Waves) shouldBe matcherFee * 2
    dex1Api.reservedBalance(alice)(usd) shouldBe amount * 2 * 7 * price / PriceConstant

    dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(2 * amount, 7 * price))
    Seq(buyOrder1, buyOrder2).foreach(order => dex1Api.cancel(alice, order))
  }

  // offset is 18, after test - 22
  "Matching on the same price level" in {
    val bobUsdBalance     = wavesNode1Api.balance(bob, usd)
    val bobWctBalance     = wavesNode1Api.balance(bob, wct)
    val aliceUsdBalance   = wavesNode1Api.balance(alice, usd)
    val aliceWctBalance   = wavesNode1Api.balance(alice, wct)
    val bobWavesBalance   = wavesNode1Api.balance(bob, Waves)
    val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)

    val sellOrder = mkOrder(bob, wctUsdPair, SELL, amount, 4 * price, matcherFee)
    placeAndAwait(sellOrder)

    dex1Api.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))

    val anotherSellOrder = mkOrder(bob, wctUsdPair, SELL, amount, 3 * price, matcherFee)
    placeAndAwait(anotherSellOrder)

    val buyOrder = mkOrder(alice, wctUsdPair, BUY, amount, 7 * price, matcherFee)
    placeAndAwait(buyOrder, OrderStatus.Filled)

    waitForOrderAtNode(buyOrder)

    dex1Api.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 5 * price))
    dex1Api.orderBook(wctUsdPair).bids shouldBe Nil

    wavesNode1Api.balance(bob, usd) shouldBe (bobUsdBalance + 4 * amount)
    wavesNode1Api.balance(bob, wct) shouldBe (bobWctBalance - amount)
    wavesNode1Api.balance(alice, usd) shouldBe (aliceUsdBalance - 4 * amount)
    wavesNode1Api.balance(alice, wct) shouldBe (aliceWctBalance + amount)

    wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance - matcherFee)
    wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - matcherFee)

    dex1Api.cancel(bob, anotherSellOrder)
  }

  // offset is 22, after test - 25
  "Matching with old orders after tick size enabled" in {
    val bobUsdBalance     = wavesNode1Api.balance(bob, usd)
    val bobWctBalance     = wavesNode1Api.balance(bob, wct)
    val aliceUsdBalance   = wavesNode1Api.balance(alice, usd)
    val aliceWctBalance   = wavesNode1Api.balance(alice, wct)
    val bobWavesBalance   = wavesNode1Api.balance(bob, Waves)
    val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)

    val sellOrder = mkOrder(bob, wctUsdPair, SELL, amount, 15 * price, matcherFee)
    placeAndAwait(sellOrder)

    dex1Api.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 20 * price))

    val buyOrder = mkOrder(alice, wctUsdPair, BUY, 2 * amount, 20 * price, matcherFee)
    placeAndAwait(buyOrder, OrderStatus.PartiallyFilled)
    dex1Api.waitForOrderStatus(sellOrder, OrderStatus.Filled)

    waitForOrderAtNode(buyOrder)

    wavesNode1Api.balance(bob, usd) shouldBe (bobUsdBalance + 15 * amount)
    wavesNode1Api.balance(bob, wct) shouldBe (bobWctBalance - amount)
    wavesNode1Api.balance(alice, usd) shouldBe (aliceUsdBalance - 15 * amount)
    wavesNode1Api.balance(alice, wct) shouldBe (aliceWctBalance + amount)
    wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance - matcherFee)
    wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - matcherFee / 2)

    withClue("partially filled order cancellation") {
      dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 12 * price))
      dex1Api.reservedBalance(alice)(Waves) shouldBe matcherFee / 2
      dex1Api.reservedBalance(alice)(usd) shouldBe 20 * price * amount / PriceConstant
      dex1Api.cancel(alice, buyOrder)
      dex1Api.reservedBalance(alice) shouldBe Map()
      dex1Api.orderBook(wctUsdPair).bids shouldBe List()
    }
  }

  // offset is 25, after test - 29
  "Matching orders of same price but neighbors levels" in {
    val bestAskOrderId = mkOrder(alice, wctUsdPair, SELL, amount, 17 * price, matcherFee)
    dex1Api.place(bestAskOrderId)

    dex1Api.orderBook(wctUsdPair).asks shouldBe Seq(LevelResponse(amount, 24 * price))

    val bestBidOrderId = mkOrder(bob, wctUsdPair, BUY, amount, 17 * price, matcherFee)
    dex1Api.place(bestBidOrderId)

    dex1Api.orderBook(wctUsdPair).bids shouldBe Seq(LevelResponse(amount, 12 * price))

    dex1Api.cancel(alice, bestAskOrderId)
    dex1Api.cancel(bob, bestBidOrderId)
  }

  "Placing order on level 0" in {
    dex1Api.tryPlace(mkOrder(bob, wctUsdPair, BUY, amount * 100000000L, 1, matcherFee)) should failWith(
      9441286, // OrderInvalidPriceLevel
      "The buy order's price 0.00000001 does not meet matcher's requirements: price >= 12 (actual tick size). Orders can not be placed into level with price 0"
    )
  }

  "Placing order on level 0 with virgin orderbook" in {
    dex1Api.tryPlace(mkOrder(bob, wctWavesPair, BUY, amount * 100000000L, 1 * 1000000L, matcherFee)) should failWith(
      9441286, // OrderInvalidPriceLevel
      "The buy order's price 0.00000001 does not meet matcher's requirements: price >= 12 (actual tick size). Orders can not be placed into level with price 0"
    )
  }

  "Matching orders with different decimals" in {
    Seq((wctUsdPair, amount, price), (wctWavesPair, amount, price * 1000000L), (wavesUsdPair, 1.waves, 100L), (wavesBtcPair, amount, price))
      .foreach {
        case (pair: AssetPair, amount: Long, price: Long) =>
          withClue(pair) {
            val aliceAmountBalance = assetBalance(alice, pair.amountAsset)
            val bobAmountBalance   = assetBalance(bob, pair.amountAsset)
            val alicePriceBalance  = assetBalance(alice, pair.priceAsset)
            val bobPriceBalance    = assetBalance(bob, pair.priceAsset)
            val aliceWavesBalance  = assetBalance(alice, Waves)
            val bobWavesBalance    = assetBalance(bob, Waves)

            val bestAskOrderId = mkOrder(alice, pair, SELL, amount, 17 * price, matcherFee)
            dex1Api.place(bestAskOrderId)

            dex1Api.orderBook(pair).asks shouldBe Seq(LevelResponse(amount, 24 * price))
            val bestBidOrderId = mkOrder(bob, pair, BUY, amount, 17 * price, matcherFee)
            dex1Api.place(bestBidOrderId)

            dex1Api.orderBook(pair).bids shouldBe Seq(LevelResponse(amount, 12 * price))

            dex1Api.cancel(alice, bestAskOrderId)
            dex1Api.cancel(bob, bestBidOrderId)

            val filledOrderId = mkOrder(bob, pair, BUY, amount, 25 * price, matcherFee)
            dex1Api.place(filledOrderId)

            val partiallyFilledOrderId = mkOrder(alice, pair, SELL, 2 * amount, 17 * price, matcherFee)
            dex1Api.place(partiallyFilledOrderId)

            dex1Api.waitForOrderStatus(filledOrderId, OrderStatus.Filled)
            dex1Api.waitForOrderStatus(partiallyFilledOrderId, OrderStatus.PartiallyFilled)
            waitForOrderAtNode(filledOrderId)

            pair match {
              case `wctUsdPair` =>
                assetBalance(alice, pair.amountAsset) shouldBe aliceAmountBalance - amount
                assetBalance(bob, pair.amountAsset) shouldBe bobAmountBalance + amount
                assetBalance(alice, pair.priceAsset) shouldBe alicePriceBalance + 25000L
                assetBalance(bob, pair.priceAsset) shouldBe bobPriceBalance - 25000L
                assetBalance(alice, Waves) shouldBe aliceWavesBalance - matcherFee / 2
                assetBalance(bob, Waves) shouldBe bobWavesBalance - matcherFee
              case `wctWavesPair` =>
                assetBalance(alice, pair.amountAsset) shouldBe aliceAmountBalance - amount
                assetBalance(bob, pair.amountAsset) shouldBe bobAmountBalance + amount
                assetBalance(alice, pair.priceAsset) shouldBe alicePriceBalance + 25000000000L - matcherFee / 2
                assetBalance(bob, pair.priceAsset) shouldBe bobPriceBalance - 25000000000L - matcherFee
              case `wavesUsdPair` =>
                assetBalance(alice, pair.amountAsset) shouldBe aliceAmountBalance - amount - matcherFee / 2
                assetBalance(bob, pair.amountAsset) shouldBe bobAmountBalance + amount - matcherFee
                assetBalance(alice, pair.priceAsset) shouldBe alicePriceBalance + 2500L
                assetBalance(bob, pair.priceAsset) shouldBe bobPriceBalance - 2500L
              case `wavesBtcPair` =>
                assetBalance(alice, pair.amountAsset) shouldBe aliceAmountBalance - amount - matcherFee / 2
                assetBalance(bob, pair.amountAsset) shouldBe bobAmountBalance + amount - matcherFee
                assetBalance(alice, pair.priceAsset) shouldBe alicePriceBalance + 25000L
                assetBalance(bob, pair.priceAsset) shouldBe bobPriceBalance - 25000L
              case _ => ???
            }

            dex1Api.cancel(alice, partiallyFilledOrderId)
          }
      }
  }
}
