package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

/**
  * BUY orders price:  (1 - p) * best bid <= price <= (1 + l) * best ask
  * SELL orders price: (1 - l) * best bid <= price <= (1 + p) * best ask
  *
  * where:
  *
  *   p = max price deviation profit / 100
  *   l = max price deviation loss / 100
  *   best bid = highest price of buy
  *   best ask = lowest price of sell
  *
  * BUY orders fee:  fee >= fs * (1 - fd) * best ask * amount
  * SELL orders fee: fee >= fs * (1 - fd) * best bid * amount
  *
  * where:
  *
  *   fs = fee in percents from order-fee settings (order-fee.percent.min-fee) / 100
  *   fd = max fee deviation / 100
  *   best bid = highest price of buy
  *   best ask = lowest price of sell
  */
class OrderDeviationsTestSuite extends MatcherSuiteBase {

  val deviationProfit = 70
  val deviationLoss   = 60
  val deviationFee    = 40

  val trueScript = Some(ExprScript(Terms.TRUE).explicitGet())

  val scriptAssetTx = mkIssue(alice, "asset1", defaultAssetQuantity, fee = smartIssueFee, script = trueScript)
  val scriptAsset   = IssuedAsset(scriptAssetTx.id())

  val anotherScriptAssetTx = mkIssue(alice, "asset2", defaultAssetQuantity, fee = smartIssueFee, script = trueScript)
  val anotherScriptAsset   = IssuedAsset(anotherScriptAssetTx.id())

  val scriptAssetsPair: AssetPair = createAssetPair(scriptAsset, anotherScriptAsset)

  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  max-price-deviations {
       |    enable = yes
       |    profit = $deviationProfit
       |    loss = $deviationLoss
       |    fee = $deviationFee
       |  }
       |  order-fee {
       |    mode = "percent"
       |    percent {
       |      asset-type = "price"
       |      min-fee = 0.1
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNode1Container(), wavesNode1Api)

    broadcastAndAwait(IssueBtcTx, IssueEthTx, IssueUsdTx, scriptAssetTx, anotherScriptAssetTx)
    Seq(scriptAsset, anotherScriptAsset).foreach { asset =>
      broadcastAndAwait(
        mkTransfer(alice, bob, defaultAssetQuantity / 2, asset, 0.005.waves)
      )
    }

    startAndWait(dex1Container(), dex1Api)
  }

  def orderIsOutOfDeviationBounds(price: String, orderType: OrderType): String = {

    val lowerBound = orderType match {
      case SELL => 100 - deviationLoss
      case BUY  => 100 - deviationProfit
    }

    val upperBound = orderType match {
      case SELL => 100 + deviationProfit
      case BUY  => 100 + deviationLoss
    }

    s"The $orderType order's price $price is out of deviation bounds. It should meet the following matcher's requirements: " +
      s"$lowerBound% of best bid price <= order price <= $upperBound% of best ask price"
  }

  def feeIsOutOfDeviationBounds(fee: String, feeAssetId: String, orderType: OrderType): String = {

    val marketType = orderType match {
      case SELL => "bid"
      case BUY  => "ask"
    }

    s"The $orderType order's matcher fee $fee $feeAssetId is out of deviation bounds. " +
      s"It should meet the following matcher's requirements: matcher fee >= ${100 - deviationFee}% of fee which should be paid in case of matching with best $marketType"
  }

  "buy orders price is" - {
    "in deviation bounds" - {
      for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
        val bestAskOrder = mkOrder(alice, assetPair, SELL, 2000.waves, 500000, 4 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        val bestBidOrder = mkOrder(bob, assetPair, BUY, 2000.waves, 300000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)

        dex1Api.place(bestAskOrder)
        dex1Api.place(bestBidOrder)

        Seq(bestAskOrder, bestBidOrder) foreach { order =>
          dex1Api.waitForOrderStatus(order, OrderStatus.Accepted)
        }

        dex1Api.orderBook(assetPair).asks shouldBe List(LevelResponse(2000.waves, 500000))
        dex1Api.orderBook(assetPair).bids shouldBe List(LevelResponse(2000.waves, 300000))

        Seq(90000 -> OrderStatus.Accepted, 800000 -> OrderStatus.Filled).foreach {
          case (price, status) =>
            val order = mkOrder(bob, assetPair, BUY, 1000.waves, price, 3 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
            placeAndAwait(order, status)
        }

        waitForOrderAtNode(bestAskOrder)

        dex1Api.reservedBalance(alice) shouldBe Map(assetPair.amountAsset -> 100000000000L)
        dex1Api.reservedBalance(bob) shouldBe Map(assetPair.priceAsset    -> 691500000L)

        cancelAll(alice, bob)
      }

      s"$wavesUsdPair" in {
        val bestAskOrder = mkOrder(bob, wavesUsdPair, SELL, 2000.waves, 500, 4 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
        val bestBidOrder = mkOrder(alice, wavesUsdPair, BUY, 2000.waves, 300, 2 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)

        dex1Api.place(bestAskOrder)
        dex1Api.place(bestBidOrder)

        Seq(bestAskOrder, bestBidOrder) foreach { order =>
          dex1Api.waitForOrderStatus(order, OrderStatus.Accepted)
        }

        dex1Api.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(2000.waves, 500))
        dex1Api.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(2000.waves, 300))

        Seq(90 -> OrderStatus.Accepted, 800 -> OrderStatus.Filled).foreach {
          case (price, status) =>
            val order = mkOrder(alice, wavesUsdPair, BUY, 1000.waves, price, 3 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
            placeAndAwait(order, status)
        }

        waitForOrderAtNode(bestAskOrder)

        dex1Api.reservedBalance(bob) shouldBe Map(wavesUsdPair.amountAsset  -> 100000000000L)
        dex1Api.reservedBalance(alice) shouldBe Map(wavesUsdPair.priceAsset -> 691500L)

        cancelAll(alice, bob)
      }
    }

    "out of deviation bounds" - {
      "-- too low" - {
        for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
          val bestBidOrder = mkOrder(bob, assetPair, BUY, 1000.waves, 300000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
          placeAndAwait(bestBidOrder)

          dex1Api.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 300000))
          dex1Api.reservedBalance(bob)(assetPair.priceAsset) shouldBe 300600000L

          dex1Api.tryPlace(mkOrder(bob, assetPair, BUY, 1000.waves, 89999, matcherFee, matcherFeeAssetId = assetPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("0.00089999", BUY)
          )

          dex1Api.reservedBalance(bob) shouldBe Map(assetPair.priceAsset -> 300600000L)

          dex1Api.cancel(bob, bestBidOrder)

          dex1Api.reservedBalance(bob) shouldBe empty
          cancelAll(alice)
        }

        s"$wavesUsdPair" in {
          val bestBidOrder = mkOrder(bob, wavesUsdPair, BUY, 1000.waves, 300, 2 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
          placeAndAwait(bestBidOrder)

          dex1Api.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 300))

          dex1Api.reservedBalance(bob)(wavesUsdPair.priceAsset) shouldBe 300600L

          dex1Api.tryPlace(mkOrder(bob, wavesUsdPair, BUY, 1000.waves, 89, matcherFee, matcherFeeAssetId = wavesUsdPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("0.89", BUY)
          )

          dex1Api.reservedBalance(bob) shouldBe Map(wavesUsdPair.priceAsset -> 300600L)

          cancelAll(alice, bob)
        }
      }

      "-- too high" - {
        for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
          val bestAskOrder = mkOrder(alice, assetPair, SELL, 1000.waves, 500000, 4 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
          placeAndAwait(bestAskOrder)

          dex1Api.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

          dex1Api
            .tryPlace(mkOrder(bob, assetPair, BUY, 1000.waves, 800001, 3 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("0.00800001", BUY)
          )

          dex1Api.reservedBalance(bob) shouldBe empty

          cancelAll(alice, bob)
        }

        s"$wavesUsdPair" in {
          val bestAskOrder = mkOrder(bob, wavesUsdPair, SELL, 1000.waves, 500, 4 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
          placeAndAwait(bestAskOrder)

          dex1Api.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 500))

          dex1Api.tryPlace(mkOrder(alice, wavesUsdPair, BUY, 1000.waves, 801, 3 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("8.01", BUY)
          )

          dex1Api.reservedBalance(alice) shouldBe empty

          cancelAll(alice, bob)
        }
      }
    }
  }

  "sell orders price is" - {
    "in deviation bounds" - {
      for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
        val bestAskOrder = mkOrder(alice, assetPair, SELL, 2000.waves, 500000, 4 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        val bestBidOrder = mkOrder(bob, assetPair, BUY, 2000.waves, 300000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)

        Seq(bestAskOrder, bestBidOrder).foreach { order =>
          placeAndAwait(order)
        }

        dex1Api.orderBook(assetPair).asks shouldBe List(LevelResponse(2000.waves, 500000))
        dex1Api.orderBook(assetPair).bids shouldBe List(LevelResponse(2000.waves, 300000))

        Seq(850000 -> OrderStatus.Accepted, 120000 -> OrderStatus.Filled).foreach {
          case (price, status) =>
            val order = mkOrder(alice, assetPair, SELL, 1000.waves, price, 3 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
            placeAndAwait(order, status)
        }

        waitForOrderAtNode(bestBidOrder)

        dex1Api.reservedBalance(alice) shouldBe Map(assetPair.amountAsset -> 300000000000L)
        dex1Api.reservedBalance(bob) shouldBe Map(assetPair.priceAsset    -> 300300000L)

        cancelAll(alice, bob)
      }
    }

    "out of deviation bounds" - {
      "-- too low" - {
        for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
          val bestBidOrder = mkOrder(bob, assetPair, BUY, 1000.waves, 300000, matcherFee, matcherFeeAssetId = assetPair.priceAsset)
          placeAndAwait(bestBidOrder)

          dex1Api.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 300000))

          dex1Api.tryPlace(mkOrder(alice, assetPair, SELL, 1000.waves, 119999, matcherFee, matcherFeeAssetId = assetPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("0.00119999", SELL)
          )

          dex1Api.reservedBalance(alice) shouldBe empty

          cancelAll(bob)
        }

        s"$wavesUsdPair" in {
          val bestBidOrder = mkOrder(bob, wavesUsdPair, BUY, 1000.waves, 300, 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
          placeAndAwait(bestBidOrder)

          dex1Api.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 300))

          dex1Api.tryPlace(mkOrder(alice, wavesUsdPair, SELL, 1000.waves, 119, 300, matcherFeeAssetId = wavesUsdPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("1.19", SELL)
          )

          dex1Api.reservedBalance(alice) shouldBe empty

          cancelAll(bob, bob)
        }
      }

      "-- too high" - {
        for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
          val bestAskOrder = mkOrder(alice, assetPair, SELL, 1000.waves, 500000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
          placeAndAwait(bestAskOrder)

          dex1Api.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

          dex1Api
            .tryPlace(mkOrder(alice, assetPair, SELL, 1000.waves, 850001, 3 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("0.00850001", SELL)
          )

          dex1Api.reservedBalance(alice) shouldBe Map(assetPair.amountAsset -> 1000.waves)

          cancelAll(alice, bob)
        }

        s"$wavesUsdPair" in {
          val bestAskOrder = mkOrder(alice, wavesUsdPair, SELL, 1000.waves, 500, 2 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
          placeAndAwait(bestAskOrder)

          dex1Api.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 500))

          dex1Api.tryPlace(mkOrder(alice, wavesUsdPair, SELL, 1000.waves, 851, 3 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)) should failWith(
            9441295, // DeviantOrderPrice
            orderIsOutOfDeviationBounds("8.51", SELL)
          )

          dex1Api.reservedBalance(alice) shouldBe Map(wavesUsdPair.amountAsset -> 1000.waves)

          cancelAll(alice, bob)
        }
      }
    }
  }

  "orders fee is" - {
    "in deviation bounds" - {
      for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
        val aliceOrder1 = mkOrder(alice, assetPair, SELL, 1000.waves, 600000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        placeAndAwait(aliceOrder1)

        dex1Api.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 600000))

        val bobOrder1 = mkOrder(bob, assetPair, BUY, 1000.waves, 800000, 3 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        placeAndAwait(bobOrder1, OrderStatus.Filled)

        val aliceOrder2 = mkOrder(alice, assetPair, BUY, 1000.waves, 700000, 3 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        placeAndAwait(aliceOrder2)
        dex1Api.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 700000))

        val bobOrder2 = mkOrder(bob, assetPair, SELL, 1000.waves, 600000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        placeAndAwait(bobOrder2, OrderStatus.Filled)
        waitForOrdersAtNode(aliceOrder1, aliceOrder2)

        cancelAll(alice, bob)
      }

      s"$wavesUsdPair" in {
        val bobOrder1 = mkOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 600, matcherFeeAssetId = wavesUsdPair.priceAsset)
        placeAndAwait(bobOrder1)

        dex1Api.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 600))

        val aliceOrder1 = mkOrder(alice, wavesUsdPair, BUY, 1000.waves, 800, 3 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
        placeAndAwait(aliceOrder1, OrderStatus.Filled)

        val aliceOrder2 = mkOrder(alice, wavesUsdPair, BUY, 1000.waves, 700, 3 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
        placeAndAwait(aliceOrder2)
        dex1Api.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 700))

        val bobOrder2 = mkOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 2 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
        placeAndAwait(bobOrder2, OrderStatus.Filled)
        waitForOrdersAtNode(bobOrder1, aliceOrder1, aliceOrder2, bobOrder2)

        cancelAll(alice, bob)
      }
    }

    "out of deviation bounds" - {
      for (assetPair <- Seq(wavesBtcPair, ethWavesPair, scriptAssetsPair)) s"$assetPair" in {
        val bestAskOrder = mkOrder(alice, assetPair, SELL, 1000.waves, 600000, 2 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        placeAndAwait(bestAskOrder)

        dex1Api.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 600000))

        dex1Api.tryPlace(mkOrder(bob, assetPair, BUY, 1000.waves, 300000, 359999, matcherFeeAssetId = assetPair.priceAsset)) should failWith(
          9441551, // DeviantOrderMatcherFee
          feeIsOutOfDeviationBounds("0.00359999", assetPair.priceAssetStr, BUY)
        )

        dex1Api.cancel(alice, bestAskOrder)

        val bestBidOrder = mkOrder(bob, assetPair, BUY, 1000.waves, 1200000, 4 * matcherFee, matcherFeeAssetId = assetPair.priceAsset)
        placeAndAwait(bestBidOrder)

        dex1Api.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 1200000))

        dex1Api.tryPlace(mkOrder(alice, assetPair, SELL, 1000.waves, 600000, 719999, matcherFeeAssetId = assetPair.priceAsset)) should failWith(
          9441551, // DeviantOrderMatcherFee
          feeIsOutOfDeviationBounds("0.00719999", assetPair.priceAssetStr, SELL)
        )

        cancelAll(alice, bob)
      }

      s"$wavesUsdPair" in {
        val bestAskOrder = mkOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 2 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
        placeAndAwait(bestAskOrder)

        dex1Api.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 600))

        dex1Api.tryPlace(mkOrder(alice, wavesUsdPair, BUY, 1000.waves, 300, 359, matcherFeeAssetId = wavesUsdPair.priceAsset)) should failWith(
          9441551, // DeviantOrderMatcherFee
          feeIsOutOfDeviationBounds("3.59", wavesUsdPair.priceAssetStr, BUY)
        )

        dex1Api.cancel(bob, bestAskOrder)

        val bestBidOrder = mkOrder(alice, wavesUsdPair, BUY, 1000.waves, 1200, 4 * 300, matcherFeeAssetId = wavesUsdPair.priceAsset)
        placeAndAwait(bestBidOrder)

        dex1Api.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 1200))

        dex1Api.tryPlace(mkOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 719, matcherFeeAssetId = wavesUsdPair.priceAsset)) should failWith(
          9441551, // DeviantOrderMatcherFee
          feeIsOutOfDeviationBounds("7.19", wavesUsdPair.priceAssetStr, SELL)
        )

        cancelAll(alice, bob)
      }
    }
  }

  private def cancelAll(xs: KeyPair*): Unit         = xs.foreach(dex1Api.cancelAll(_))
  private def waitForOrdersAtNode(xs: Order*): Unit = xs.foreach(waitForOrderAtNode(_))
}
