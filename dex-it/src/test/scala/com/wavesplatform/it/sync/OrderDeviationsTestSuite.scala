package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.LevelResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.it._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

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
  val deviationLoss = 60
  val deviationFee = 40

  val Btc = IssuedAsset(BtcId)
  val trueScript = Some(Base64.encode(createBoolScript("true").bytes.apply))
  val scriptAsset: String = node.broadcastIssue(alice, "asset1", "price script asset", defaultAssetQuantity, 8, reissuable = false, smartIssueFee, trueScript).id
  val anotherScriptAsset: String = node.broadcastIssue(alice, "asset2", "another price script asset", defaultAssetQuantity, 8, reissuable = false, smartIssueFee, trueScript).id
  val scriptAssetsPair: AssetPair = createAssetPair(scriptAsset, anotherScriptAsset)

  override protected def nodeConfigs: Seq[Config] = {
    val orderDeviations =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  max-price-deviations {
         |    enable = yes
         |    profit = $deviationProfit
         |    loss = $deviationLoss
         |    fee = $deviationFee
         |  }
         |
         |  order-fee {
         |    mode = "percent"
         |
         |    percent {
         |      asset-type = "price"
         |      min-fee = 0.1
         |    }
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(ConfigFactory.parseString(orderDeviations).withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Array(IssueBtcTx, IssueEthTx, IssueUsdTx).foreach(issueTx =>
      node.waitForTransaction(node.broadcastRequest(issueTx.json()).id))
    Array(scriptAsset, anotherScriptAsset).foreach(asset =>
      node.broadcastTransfer(alice, bob.address, defaultAssetQuantity / 2, 0.005.waves, Some(asset), None, waitForTx = true))
  }

  def orderIsOutOfDeviationBounds(price: String, orderType: OrderType): String = {
    def lowBound: Int = orderType match {
      case SELL => 100 - deviationLoss
      case BUY => 100 - deviationProfit
    }
    def upperBound: Int = orderType match {
      case SELL => 100 + deviationProfit
      case BUY => 100 + deviationLoss
    }
    s"The $orderType order's price $price is out of deviation bounds. It should meet the following matcher's requirements: " +
      s"$lowBound% of best bid price <= order price <= $upperBound% of best ask price"
  }

  def feeIsOutOfDeviationBounds(fee: String, feeAssetId: String, orderType: OrderType): String = {
    def marketType: String = orderType match {
      case SELL => "bid"
      case BUY => "ask"
    }
    s"The $orderType order's matcher fee $fee $feeAssetId is out of deviation bounds. " +
      s"It should meet the following matcher's requirements: matcher fee >= ${100 - deviationFee}% of fee which should be paid in case of matching with best $marketType"
  }

  def priceAssetBalance(owner: KeyPair, assetPair: AssetPair): Long = {
    val priceAsset = assetPair.priceAssetStr
    if (priceAsset == "WAVES")
      node.accountBalances(owner.address)._1
    else {
      node.assetBalance(owner.address, priceAsset).balance
    }
  }

  def amountAssetBalance(owner: KeyPair, assetPair: AssetPair): Long = {
    val amountAsset = assetPair.amountAssetStr
    if (amountAsset == "WAVES")
      node.accountBalances(owner.address)._1
    else {
      node.assetBalance(owner.address, amountAsset).balance
    }
  }


  "buy orders price is" - {
    "in deviation bounds" in {
      for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
        val aliceBalance = amountAssetBalance(alice, assetPair)
        val bobBalance = amountAssetBalance(bob, assetPair)
        val aliceBtcBalance = priceAssetBalance(alice, assetPair)
        val bobBtcBalance = priceAssetBalance(bob, assetPair)

        val bestAskOrderId = node.placeOrder(alice, assetPair, SELL, 2000.waves, 500000, 4 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        val bestBidOrderId = node.placeOrder(bob, assetPair, BUY, 2000.waves, 300000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id

        Array(bestAskOrderId, bestBidOrderId).foreach(orderId =>
          node.waitOrderStatus(assetPair, orderId, expectedStatus = "Accepted"))
        node.orderBook(assetPair).asks shouldBe List(LevelResponse(2000.waves, 500000))
        node.orderBook(assetPair).bids shouldBe List(LevelResponse(2000.waves, 300000))

        Array(90000 -> "Accepted", 800000 -> "Filled").foreach { case (price, status) =>
          node.waitOrderStatus(assetPair, node.placeOrder(bob, assetPair, BUY, 1000.waves, price, 3 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset)
            .message.id, expectedStatus = status)
        }
        node.waitOrderInBlockchain(bestAskOrderId)

        amountAssetBalance(alice, assetPair) shouldBe aliceBalance - 1000.waves
        amountAssetBalance(bob, assetPair) shouldBe bobBalance + 1000.waves
        priceAssetBalance(alice, assetPair) shouldBe (aliceBtcBalance + 500000000L - 600000L)
        priceAssetBalance(bob, assetPair) shouldBe (bobBtcBalance - 500000000L - 562500L)
        node.reservedBalance(alice) shouldBe Map(assetPair.amountAssetStr -> 100000000000L)
        node.reservedBalance(bob) shouldBe Map(assetPair.priceAssetStr -> 691500000L)
        Array(alice, bob).foreach(sender => node.cancelAllOrders(sender))
      }

      withClue("in usd") {
        val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
        val bobBalance = amountAssetBalance(bob, wavesUsdPair)
        val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
        val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

        val bestAskOrderId = node.placeOrder(bob, wavesUsdPair, SELL, 2000.waves, 500, 4 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        val bestBidOrderId = node.placeOrder(alice, wavesUsdPair, BUY, 2000.waves, 300, 2 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id

        Array(bestAskOrderId, bestBidOrderId).foreach(orderId =>
          node.waitOrderStatus(wavesUsdPair, orderId, expectedStatus = "Accepted"))
        node.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(2000.waves, 500))
        node.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(2000.waves, 300))

        Array(90 -> "Accepted", 800 -> "Filled").foreach { case (price, status) =>
          node.waitOrderStatus(wavesUsdPair, node.placeOrder(alice, wavesUsdPair, BUY, 1000.waves, price, 3 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset)
            .message.id, expectedStatus = status)
        }
        node.waitOrderInBlockchain(bestAskOrderId)

        amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance - 1000.waves
        amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance + 1000.waves
        priceAssetBalance(bob, wavesUsdPair) shouldBe (bobBtcBalance + 500000L - 600L)
        priceAssetBalance(alice, wavesUsdPair) shouldBe (aliceBtcBalance - 500000L - 562L)
        node.reservedBalance(bob) shouldBe Map(wavesUsdPair.amountAssetStr -> 100000000000L)
        node.reservedBalance(alice) shouldBe Map(wavesUsdPair.priceAssetStr -> 691500L)
        Array(alice, bob).foreach(sender => node.cancelAllOrders(sender))
      }
    }

    "out of deviation bounds" - {
      "-- too low" in {
        for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
          val aliceBalance = amountAssetBalance(alice, assetPair)
          val bobBalance = amountAssetBalance(bob, assetPair)
          val aliceBtcBalance = priceAssetBalance(alice, assetPair)
          val bobBtcBalance = priceAssetBalance(bob, assetPair)

          val bestBidOrderId = node.placeOrder(bob, assetPair, BUY, 1000.waves, 300000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
          node.waitOrderStatus(assetPair, bestBidOrderId, expectedStatus = "Accepted")
          node.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 300000))

          node.reservedBalance(bob)(assetPair.priceAssetStr) shouldBe 300600000L

          assertBadRequestAndMessage(node.placeOrder(bob, assetPair, BUY, 1000.waves, 89999, matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset),
            orderIsOutOfDeviationBounds("0.00089999", BUY), 400)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob, assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(bob) shouldBe Map(assetPair.priceAssetStr -> 300600000L)

          node.cancelOrder(bob, wavesBtcPair, bestBidOrderId)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob, assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(bob) shouldBe empty
        }

        withClue("in usd") {
          val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
          val bobBalance = amountAssetBalance(bob, wavesUsdPair)
          val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
          val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

          val bestBidOrderId = node.placeOrder(bob, wavesUsdPair, BUY, 1000.waves, 300, 2 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
          node.waitOrderStatus(wavesUsdPair, bestBidOrderId, expectedStatus = "Accepted")
          node.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 300))

          node.reservedBalance(bob)(wavesUsdPair.priceAssetStr) shouldBe 300600L

          assertBadRequestAndMessage(node.placeOrder(bob, wavesUsdPair, BUY, 1000.waves, 89, matcherFee, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset),
            orderIsOutOfDeviationBounds("0.89", BUY), 400)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(bob) shouldBe Map(wavesUsdPair.priceAssetStr -> 300600L)

          node.cancelOrder(bob, wavesBtcPair, bestBidOrderId)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(bob) shouldBe empty
        }
      }

      "-- too high" in {
        for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
          val aliceBalance = amountAssetBalance(alice, assetPair)
          val bobBalance = amountAssetBalance(bob, assetPair)
          val aliceBtcBalance = priceAssetBalance(alice, assetPair)
          val bobBtcBalance = priceAssetBalance(bob, assetPair)

          val bestAskOrderId  = node.placeOrder(alice, assetPair, SELL, 1000.waves, 500000, 4 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
          node.waitOrderStatus(assetPair, bestAskOrderId, expectedStatus = "Accepted")
          node.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

          assertBadRequestAndMessage(node.placeOrder(bob, assetPair, BUY, 1000.waves, 800001, 3 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset),
            orderIsOutOfDeviationBounds("0.00800001", BUY), 400)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob, assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(bob) shouldBe empty
          node.cancelOrder(alice, assetPair, bestAskOrderId)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob, assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(bob) shouldBe empty
        }
        withClue("in usd") {
          val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
          val bobBalance = amountAssetBalance(bob, wavesUsdPair)
          val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
          val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

          val bestAskOrderId  = node.placeOrder(bob, wavesUsdPair, SELL, 1000.waves, 500, 4 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
          node.waitOrderStatus(wavesUsdPair, bestAskOrderId, expectedStatus = "Accepted")
          node.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 500))

          assertBadRequestAndMessage(node.placeOrder(alice, wavesUsdPair, BUY, 1000.waves, 801, 3 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset),
            orderIsOutOfDeviationBounds("8.01", BUY), 400)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
          node.cancelOrder(bob, wavesUsdPair, bestAskOrderId)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
        }
      }
    }
  }

  "sell orders price is" - {
    "in deviation bounds" in {
      for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
        val aliceBalance = amountAssetBalance(alice, assetPair)
        val bobBalance = amountAssetBalance(bob, assetPair)
        val aliceBtcBalance = priceAssetBalance(alice, assetPair)
        val bobBtcBalance = priceAssetBalance(bob, assetPair)

        val bestAskOrderId  = node.placeOrder(alice, assetPair, SELL, 2000.waves, 500000, 4 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        val bestBidOrderId = node.placeOrder(bob, assetPair, BUY, 2000.waves, 300000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id

        Array(bestAskOrderId, bestBidOrderId).foreach(orderId =>
          node.waitOrderStatus(assetPair, orderId, expectedStatus = "Accepted"))
        node.orderBook(assetPair).asks shouldBe List(LevelResponse(2000.waves, 500000))
        node.orderBook(assetPair).bids shouldBe List(LevelResponse(2000.waves, 300000))

        Array(850000 -> "Accepted", 120000 -> "Filled").foreach { case (price, status) =>
          node.waitOrderStatus(assetPair, node.placeOrder(alice, assetPair, SELL, 1000.waves, price, 3 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset)
            .message.id, expectedStatus = status)
        }
        node.waitOrderInBlockchain(bestBidOrderId)

        amountAssetBalance(alice, assetPair) shouldBe aliceBalance - 1000.waves
        amountAssetBalance(bob, assetPair) shouldBe bobBalance + 1000.waves
        priceAssetBalance(alice, assetPair) shouldBe (aliceBtcBalance + 300000000L - 900000L)
        priceAssetBalance(bob, assetPair) shouldBe (bobBtcBalance - 300000000L - 300000L)
        node.reservedBalance(alice) shouldBe Map(assetPair.amountAssetStr -> 300000000000L)
        node.reservedBalance(bob) shouldBe Map(assetPair.priceAssetStr -> 300300000L)
        Array(alice, bob).foreach(sender => node.cancelAllOrders(sender))
      }
    }

    "out of deviation bounds" - {
      "-- too low" in {
        for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
          val aliceBalance = amountAssetBalance(alice, assetPair)
          val bobBalance = amountAssetBalance(bob, assetPair)
          val aliceBtcBalance = priceAssetBalance(alice, assetPair)
          val bobBtcBalance = priceAssetBalance(bob, assetPair)

          val bestBidOrderId = node.placeOrder(bob, assetPair, BUY, 1000.waves, 300000, matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
          node.waitOrderStatus(assetPair, bestBidOrderId, expectedStatus = "Accepted")
          node.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 300000))

          assertBadRequestAndMessage(node.placeOrder(alice, assetPair, SELL, 1000.waves, 119999, matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset),
            orderIsOutOfDeviationBounds("0.00119999", SELL), 400)
          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob,assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
          node.cancelOrder(bob, assetPair, bestBidOrderId)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob,assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
        }

        withClue("in usd") {
          val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
          val bobBalance = amountAssetBalance(bob, wavesUsdPair)
          val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
          val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

          val bestBidOrderId = node.placeOrder(bob, wavesUsdPair, BUY, 1000.waves, 300, 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
          node.waitOrderStatus(wavesUsdPair, bestBidOrderId, expectedStatus = "Accepted")
          node.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 300))

          assertBadRequestAndMessage(node.placeOrder(alice, wavesUsdPair, SELL, 1000.waves, 119, 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset),
            orderIsOutOfDeviationBounds("1.19", SELL), 400)
          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
          node.cancelOrder(bob, wavesUsdPair, bestBidOrderId)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
        }
      }

      "-- too high" in {
        for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
          val aliceBalance = amountAssetBalance(alice, assetPair)
          val bobBalance = amountAssetBalance(bob, assetPair)
          val aliceBtcBalance = priceAssetBalance(alice, assetPair)
          val bobBtcBalance = priceAssetBalance(bob, assetPair)

          val bestAskOrderId = node.placeOrder(alice, assetPair, SELL, 1000.waves, 500000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
          node.waitOrderStatus(assetPair, bestAskOrderId, expectedStatus = "Accepted")
          node.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 500000))

          assertBadRequestAndMessage(node.placeOrder(alice, assetPair, SELL, 1000.waves, 850001, 3 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset),
            orderIsOutOfDeviationBounds("0.00850001", SELL), 400)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob, assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe Map(assetPair.amountAssetStr -> 1000.waves)
          node.cancelOrder(alice, assetPair, bestAskOrderId)

          amountAssetBalance(alice, assetPair) shouldBe aliceBalance
          amountAssetBalance(bob, assetPair) shouldBe bobBalance
          priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
        }

        withClue("in usd") {
          val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
          val bobBalance = amountAssetBalance(bob, wavesUsdPair)
          val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
          val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

           val bestAskOrderId = node.placeOrder(alice, wavesUsdPair, SELL, 1000.waves, 500, 2 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
          node.waitOrderStatus(wavesUsdPair, bestAskOrderId, expectedStatus = "Accepted")
          node.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 500))

          assertBadRequestAndMessage(node.placeOrder(alice, wavesUsdPair, SELL, 1000.waves, 851, 3 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset),
            orderIsOutOfDeviationBounds("8.51", SELL), 400)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe Map(wavesUsdPair.amountAssetStr -> 1000.waves)
          node.cancelOrder(alice, wavesUsdPair, bestAskOrderId)

          amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
          amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
          priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
          priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
          node.reservedBalance(alice) shouldBe empty
        }
      }
    }
  }

  "orders fee is" - {
    "in deviation bounds" in {
      for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
        val aliceBalance = amountAssetBalance(alice, assetPair)
        val bobBalance = amountAssetBalance(bob, assetPair)
        val aliceBtcBalance = priceAssetBalance(alice, assetPair)
        val bobBtcBalance = priceAssetBalance(bob, assetPair)

        val bestAskOrderId = node.placeOrder(alice, assetPair, SELL, 1000.waves, 600000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        node.waitOrderStatus(assetPair, bestAskOrderId, expectedStatus = "Accepted")
        node.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 600000))

        val bobOrderId = node.placeOrder(bob, assetPair, BUY, 1000.waves, 800000, 3 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        node.waitOrderStatus(assetPair, bobOrderId, expectedStatus = "Filled")

        val bestBidOrderId = node.placeOrder(bob, assetPair, BUY, 1000.waves, 700000, 3 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        node.waitOrderStatus(assetPair, bestBidOrderId, expectedStatus = "Accepted")
        node.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 700000))

        val aliceOrderId = node.placeOrder(alice, assetPair, SELL, 1000.waves, 600000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        node.waitOrderStatus(assetPair, aliceOrderId, expectedStatus = "Filled")

        amountAssetBalance(alice, assetPair) shouldBe aliceBalance
        amountAssetBalance(bob, assetPair) shouldBe bobBalance
        priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
        priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
      }

      withClue("in usd") {
        val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
        val bobBalance = amountAssetBalance(bob, wavesUsdPair)
        val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
        val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

        val bestAskOrderId = node.placeOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 600, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        node.waitOrderStatus(wavesUsdPair, bestAskOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 600))

        val bobOrderId = node.placeOrder(alice, wavesUsdPair, BUY, 1000.waves, 800, 3 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        node.waitOrderStatus(wavesUsdPair, bobOrderId, expectedStatus = "Filled")

        val bestBidOrderId = node.placeOrder(alice, wavesUsdPair, BUY, 1000.waves, 700, 3 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        node.waitOrderStatus(wavesUsdPair, bestBidOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 700))

        val aliceOrderId = node.placeOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 2 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        node.waitOrderStatus(wavesUsdPair, aliceOrderId, expectedStatus = "Filled")

        amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
        amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
        priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
        priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
      }
    }

    "out of deviation bounds" in {
      for (assetPair <- Array(wavesBtcPair, ethWavesPair, scriptAssetsPair)) {
        val aliceBalance = amountAssetBalance(alice, assetPair)
        val bobBalance = amountAssetBalance(bob, assetPair)
        val aliceBtcBalance = priceAssetBalance(alice, assetPair)
        val bobBtcBalance = priceAssetBalance(bob, assetPair)

        val bestAskOrderId = node.placeOrder(alice, assetPair, SELL, 1000.waves, 600000, 2 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        node.waitOrderStatus(assetPair, bestAskOrderId, expectedStatus = "Accepted")
        node.orderBook(assetPair).asks shouldBe List(LevelResponse(1000.waves, 600000))

        assertBadRequestAndMessage(node.placeOrder(bob, assetPair, BUY, 1000.waves, 300000, 359999, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id,
          feeIsOutOfDeviationBounds("0.00359999", assetPair.priceAssetStr, BUY), 400)
        node.cancelOrder(alice, assetPair, bestAskOrderId)

        val bestBidOrderId = node.placeOrder(bob, assetPair, BUY, 1000.waves, 1200000, 4 * matcherFee, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id
        node.waitOrderStatus(assetPair, bestBidOrderId, expectedStatus = "Accepted")
        node.orderBook(assetPair).bids shouldBe List(LevelResponse(1000.waves, 1200000))

        assertBadRequestAndMessage(node.placeOrder(alice, assetPair, SELL, 1000.waves, 600000, 719999, version = 3, matcherFeeAssetId = assetPair.priceAsset).message.id,
          feeIsOutOfDeviationBounds("0.00719999", assetPair.priceAssetStr, SELL), 400)
        node.cancelOrder(bob, assetPair, bestBidOrderId)

        amountAssetBalance(alice, assetPair) shouldBe aliceBalance
        amountAssetBalance(bob, assetPair) shouldBe bobBalance
        priceAssetBalance(alice, assetPair) shouldBe aliceBtcBalance
        priceAssetBalance(bob, assetPair) shouldBe bobBtcBalance
      }

      withClue("in isd") {
        val aliceBalance = amountAssetBalance(alice, wavesUsdPair)
        val bobBalance = amountAssetBalance(bob, wavesUsdPair)
        val aliceBtcBalance = priceAssetBalance(alice, wavesUsdPair)
        val bobBtcBalance = priceAssetBalance(bob, wavesUsdPair)

        val bestAskOrderId = node.placeOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 2 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        node.waitOrderStatus(wavesUsdPair, bestAskOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesUsdPair).asks shouldBe List(LevelResponse(1000.waves, 600))

        assertBadRequestAndMessage(node.placeOrder(alice, wavesUsdPair, BUY, 1000.waves, 300, 359, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id,
          feeIsOutOfDeviationBounds("3.59", wavesUsdPair.priceAssetStr, BUY), 400)
        node.cancelOrder(bob, wavesUsdPair, bestAskOrderId)

        val bestBidOrderId = node.placeOrder(alice, wavesUsdPair, BUY, 1000.waves, 1200, 4 * 300, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id
        node.waitOrderStatus(wavesUsdPair, bestBidOrderId, expectedStatus = "Accepted")
        node.orderBook(wavesUsdPair).bids shouldBe List(LevelResponse(1000.waves, 1200))

        assertBadRequestAndMessage(node.placeOrder(bob, wavesUsdPair, SELL, 1000.waves, 600, 719, version = 3, matcherFeeAssetId = wavesUsdPair.priceAsset).message.id,
          feeIsOutOfDeviationBounds("7.19", wavesUsdPair.priceAssetStr, SELL), 400)
        node.cancelOrder(alice, wavesUsdPair, bestBidOrderId)

        amountAssetBalance(alice, wavesUsdPair) shouldBe aliceBalance
        amountAssetBalance(bob, wavesUsdPair) shouldBe bobBalance
        priceAssetBalance(alice, wavesUsdPair) shouldBe aliceBtcBalance
        priceAssetBalance(bob, wavesUsdPair) shouldBe bobBtcBalance
      }
    }
  }
}
