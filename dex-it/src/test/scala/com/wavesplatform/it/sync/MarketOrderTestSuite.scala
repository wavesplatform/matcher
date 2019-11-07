package com.wavesplatform.it.sync

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.MatcherStatusResponse
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

class MarketOrderTestSuite extends MatcherSuiteBase {
  val fixedFee   = 0.003.waves
  val percentFee = 14

  implicit class DoubleOps(value: Double) {
    val waves, eth: Long = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long        = Normalization.normalizePrice(value, 8, 2)
  }

  def tooLowPrice(orderType: String, price: String): String = {
    s"Price of the $orderType market order ($price) is too low for its full execution with the current market state"
  }

  def tooHighPrice(orderType: String, price: String): String = {
    s"Price of the $orderType market order ($price) is too high for its full execution with the current market state"
  }

  override protected def nodeConfigs: Seq[Config] = {
    val orderFeeSettingsStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = percent
         |    dynamic {
         |      base-fee = 300000
         |    }
         |    percent {
         |      asset-type = amount
         |      min-fee = 0.1
         |    }
         |    fixed {
         |      asset =  WAVES
         |      min-fee = 300000
         |    }
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val txIds = Seq(IssueWctTx, IssueUsdTx, IssueEthTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))
    node.waitForTransaction(node.broadcastTransfer(alice, bob.toAddress.toString, 10000.usd, fixedFee, Some(UsdId.toString), None).id)
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    node.cancelAllOrders(alice)
    node.cancelAllOrders(bob)
  }

  def placeOrders(sender: KeyPair, pair: AssetPair, orderType: OrderType, feeMode: String = "fixed")(orders: (Long, Long)*): Unit = {
    orders.zipWithIndex.foreach {
      case ((amount, price), idx) =>
        node.placeOrder(sender = sender,
                        pair = pair,
                        orderType = orderType,
                        amount = amount,
                        price = price,
                        version = 3: Byte,
                        fee = getFee(feeMode),
                        timestamp = System.currentTimeMillis + idx)
    }
  }

  def placeMarketOrder(sender: KeyPair, pair: AssetPair, orderType: OrderType, amount: Long, price: Long): MatcherStatusResponse = {
    node.waitOrderStatus(pair, node.placeMarketOrder(node.prepareOrder(sender, pair, orderType, amount, price, fee = fixedFee)).message.id, "Filled")
  }

  def createAccountWithBalance(balances: (Long, Option[String])*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) => {
        if (asset != None)
          assert(
            node.assetBalance(alice.toAddress.toString, asset.get.toString).balance >= balance,
            s"Alice doesn't have enough balance in ${asset.get.toString} to make a transfer"
          )
        node.waitForTransaction(node.broadcastTransfer(alice, account.toAddress.toString, balance, fixedFee, asset, None).id)
      }
    }
    account
  }

  def getFee(mode: String): Long = {
    mode match {
      case "percent" => percentFee.waves
      case "fixed"   => fixedFee
      case _         => 0L
    }
  }

  def calculateFeeValue(amount: Long, feeMode: String): Long = {
    feeMode match {
      case "percent" => amount / 100 * percentFee
      case "fixed"   => fixedFee
    }
  }

  "Processing market orders" - {
    def testSellFilledMarketOrder(feeMode: String): Unit = {
      val amount = 100.waves
      val price  = 1.usd

      val account1 = createAccountWithBalance(200.usd   -> Some(UsdId.toString))
      val account2 = createAccountWithBalance(200.waves -> None)

      placeOrders(account1, wavesUsdPair, BUY, feeMode)(amount -> price)

      val marketOrder = node.placeMarketOrder(node.prepareOrder(account2, wavesUsdPair, SELL, amount, price, getFee(feeMode)))
      node.waitOrderProcessed(wavesUsdPair, marketOrder.message.id)
      node.waitOrderStatusAndAmount(wavesUsdPair, marketOrder.message.id, "Filled", Some(amount))

      node.accountBalances(account1.toAddress.toString)._1 should be(amount - calculateFeeValue(amount, feeMode))
      node.assetBalance(account1.toAddress.toString, UsdId.toString).balance should be(200.usd - price * amount / 1.waves)

      node.accountBalances(account2.toAddress.toString)._1 should be(amount - calculateFeeValue(amount, feeMode))
      node.assetBalance(account2.toAddress.toString, UsdId.toString).balance should be(price * amount / 1.waves)
    }

    def testBuyFilledMarketOrder(feeMode: String): Unit = {
      val amount = 100.waves
      val price  = 1.usd

      val account1 = createAccountWithBalance(200.waves -> None)
      val account2 = createAccountWithBalance(200.usd   -> Some(UsdId.toString))

      placeOrders(account1, wavesUsdPair, SELL, feeMode)(amount -> price)

      val marketOrder = node.placeMarketOrder(node.prepareOrder(account2, wavesUsdPair, BUY, amount, price, getFee(feeMode)))
      node.waitOrderProcessed(wavesUsdPair, marketOrder.message.id)
      node.waitOrderStatusAndAmount(wavesUsdPair, marketOrder.message.id, "Filled", Some(amount))

      node.accountBalances(account1.toAddress.toString)._1 should be(amount - calculateFeeValue(amount, feeMode))
      node.assetBalance(account1.toAddress.toString, UsdId.toString).balance should be(200.usd - price * amount / 1.waves)

      node.accountBalances(account2.toAddress.toString)._1 should be(amount - calculateFeeValue(amount, feeMode))
      node.assetBalance(account2.toAddress.toString, UsdId.toString).balance should be(price * amount / 1.waves)
    }

    "percent fee mode" - {
      val feeMode = "percent"

      "processing market order (SELL)" in {
        testSellFilledMarketOrder(feeMode)
      }

      "processing market order (BUY)" in {
        testBuyFilledMarketOrder(feeMode)
      }
    }

    "fixed fee mode" - {
      val feeMode = "fixed"
      docker.restartNode(node, ConfigFactory.parseString("waves.dex.order-fee.mode = fixed"))

      "processing market order (SELL)" in {
        testSellFilledMarketOrder(feeMode)
      }

      "processing market order (BUY)" in {
        testBuyFilledMarketOrder(feeMode)
      }
    }

    "should be matched with order having the best price (BUY)" in {
      val marketPrice = 0.4.usd
      val amount      = 25.waves
      val bestPrice   = 0.3.usd

      placeOrders(alice, wavesUsdPair, SELL)(
        50.waves -> 0.33.usd,
        10.waves -> bestPrice,
        25.waves -> 0.35.usd,
      )

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, BUY, amount, marketPrice, fixedFee)).message.id)

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids should be(empty)
      orderBook.asks shouldNot be(empty)

      orderBook.asks.filter(order => order.price == bestPrice) should be(empty)
      orderBook.asks should have size 2
    }

    "should be matched with order having the best price (SELL)" in {
      val marketPrice = 0.6.usd
      val amount      = 25.waves
      val bestPrice   = 0.8.usd
      val secondPrice = 0.79.usd

      placeOrders(alice, wavesUsdPair, OrderType.BUY)(
        10.waves -> bestPrice,
        50.waves -> 0.78.usd,
        25.waves -> secondPrice,
      )

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, OrderType.SELL, amount, marketPrice, fixedFee)).message.id)

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks should be(empty)

      orderBook.bids.filter(order => order.price == bestPrice) should be(empty)
      orderBook.bids.filter(order => order.price == secondPrice && order.amount == 10.waves) should have size 1
      orderBook.bids should have size 2
    }

    "should be removed from order book when the restriction by tokens count has been reached (BUY)" in {
      val bobWBefore        = node.accountBalances(bob.toAddress.toString)._1
      val bobUBefore        = node.assetBalance(bob.toAddress.toString, UsdId.toString).balance
      val marketPrice       = 0.5.usd
      val marketOrderAmount = 150.waves
      val ordersAmount      = 120.waves

      placeOrders(alice, wavesUsdPair, SELL)(
        30.waves -> 0.3.usd,
        40.waves -> 0.4.usd,
        50.waves -> 0.5.usd,
      )

      val marketOrder = node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, BUY, marketOrderAmount, marketPrice, fixedFee))
      node.waitOrderProcessed(wavesUsdPair, marketOrder.message.id)
      node.waitOrderStatusAndAmount(wavesUsdPair, marketOrder.message.id, "Filled", Some(ordersAmount))

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids should be(empty)
      orderBook.asks should be(empty)

      /* market order fee value depends on matched orders in proportion */
      node.accountBalances(bob.toAddress.toString)._1 should be(bobWBefore + ordersAmount - fixedFee * ordersAmount / marketOrderAmount)

      node.assetBalance(bob.toAddress.toString, UsdId.toString).balance should be(
        bobUBefore - 0.3.usd * 30.waves / 1.waves - 0.4.usd * 40.waves / 1.waves - 0.5.usd * 50.waves / 1.waves)
    }

    "should be removed from order book when the restriction by tokens count has been reached (SELL)" in {
      // val bobWBefore        = node.accountBalances(bob.toAddress.toString)._1
      val bobUBefore        = node.assetBalance(bob.toAddress.toString, UsdId.toString).balance
      val marketPrice       = 0.1.usd
      val marketOrderAmount = 72.waves
      val ordersAmount      = 36.waves

      placeOrders(alice, wavesUsdPair, OrderType.BUY)(
        10.waves -> 0.2.usd,
        14.waves -> 0.3.usd,
        12.waves -> 0.4.usd,
      )

      val marketOrder = node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, OrderType.SELL, marketOrderAmount, marketPrice, fixedFee))
      node.waitOrderProcessed(wavesUsdPair, marketOrder.message.id)
      node.waitOrderStatusAndAmount(wavesUsdPair, marketOrder.message.id, "Filled", Some(ordersAmount))

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids should be(empty)
      orderBook.asks should be(empty)

      /* market order fee value depends on matched orders in proportion
       *
       *  TODO: bug DEX-454
       *  node.accountBalances(bob.toAddress.toString)._1 should be(bobWBefore - ordersAmount - fee * ordersAmount / marketOrderAmount)
       */

      node.assetBalance(bob.toAddress.toString, UsdId.toString).balance should be(
        bobUBefore + 0.2.usd * 10.waves / 1.waves + 0.3.usd * 14.waves / 1.waves + 0.4.usd * 12.waves / 1.waves)

    }

    "should be removed from order book when there are no suitable orders by limit of the price (BUY)" in {
      val accountBalanceWBefore = 1.waves
      val marketOrderAmount     = 200.waves
      val marketPrice           = 0.1.usd
      val anotherOrderAmount    = 1.waves

      val account = createAccountWithBalance(
        1.waves -> None,
        100.usd -> Some(UsdId.toString)
      )

      val creationTime = System.currentTimeMillis

      node.placeOrder(node.prepareOrder(alice, wavesUsdPair, OrderType.SELL, 200.waves, marketPrice, fixedFee)).message.id
      node.placeOrder(node.prepareOrder(bob, wavesUsdPair, BUY, 1.waves, marketPrice, fixedFee * 2, creationTime = creationTime)).message.id
      node.waitOrderProcessed(
        wavesUsdPair,
        node
          .placeMarketOrder(node.prepareOrder(account, wavesUsdPair, BUY, marketOrderAmount, marketPrice, fixedFee, creationTime = creationTime))
          .message
          .id
      )

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids should be(empty)
      orderBook.asks should be(empty)

      node.accountBalances(account.toAddress.toString)._1 should be(
        accountBalanceWBefore + marketOrderAmount - anotherOrderAmount - fixedFee * (marketOrderAmount - anotherOrderAmount) / marketOrderAmount)
    }

    "should be accepted if price * amount > current balance, but it can be filled by offers with lower price" in {
      val marketOrderAmount = 150.waves
      val marketOrderPrice  = 1.usd
      val accountUsdBalance = 100.usd
      val account = createAccountWithBalance(
        accountUsdBalance -> Some(UsdId.toString)
      )

      placeOrders(alice, wavesUsdPair, SELL)(
        5.waves   -> 0.2.usd,
        15.waves  -> 0.3.usd,
        30.waves  -> 0.4.usd,
        100.waves -> 0.5.usd,
        150.waves -> 2.usd,
      )

      val marketOrder = node.placeMarketOrder(node.prepareOrder(account, wavesUsdPair, BUY, marketOrderAmount, marketOrderPrice, fixedFee))
      node.waitOrderProcessed(wavesUsdPair, marketOrder.message.id)
      node.waitOrderStatusAndAmount(wavesUsdPair, marketOrder.message.id, "Filled", Some(marketOrderAmount))

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.asks should have size 1
      orderBook.bids should be(empty)

      node.accountBalances(account.toAddress.toString)._1 should be(marketOrderAmount - fixedFee)
      node.assetBalance(account.toAddress.toString, UsdId.toString).balance should be(
        accountUsdBalance - 5 * 0.2.usd - 15 * 0.3.usd - 30 * 0.4.usd - 100 * 0.5.usd)
    }
  }

  "Validation market orders" - {
    val price = 0.4.usd

    "should be accepted if there is no way to fill it completely (sum of all orders in order book < amount of market order" in {
      placeOrders(bob, wavesUsdPair, SELL)(100.waves -> price)

      val total = node.orderBook(wavesUsdPair).bids.map(_.amount).sum

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeMarketOrder(node.prepareOrder(alice, wavesUsdPair, BUY, total + 1000.waves, price, fixedFee)).message.id)
    }

    "should be accepted if user doesn't have enough Waves to pay fee, but he take waves from order result " in {
      val amount   = 100.waves
      val price    = 0.1.usd
      val transfer = 100.usd

      val account = createAccountWithBalance {
        transfer -> Some(UsdId.toString)
      }

      placeOrders(alice, wavesUsdPair, SELL)(
        amount -> price
      )

      node.waitOrderProcessed(wavesUsdPair, node.placeMarketOrder(node.prepareOrder(account, wavesUsdPair, BUY, amount, price, fixedFee)).message.id)

      node.accountBalances(account.toAddress.toString)._1 should be(amount - fixedFee)
    }

    "should be rejected if the price is too low for completely filling by current opened orders (BUY)" in {
      val amount      = 100.waves
      val marketPrice = price - 0.1.usd

      placeOrders(bob, wavesUsdPair, SELL)(amount -> price)

      assertBadRequestAndMessage(node.placeMarketOrder(node.prepareOrder(alice, wavesUsdPair, BUY, amount, marketPrice, fixedFee)),
                                 tooLowPrice("buy", "0.3"))
    }

    "should be rejected if the price is too high for completely filling by current opened orders (SELL)" in {
      val amount      = 100.waves
      val marketPrice = price + 0.1.usd

      placeOrders(bob, wavesUsdPair, OrderType.BUY)(amount -> price)

      assertBadRequestAndMessage(node.placeMarketOrder(node.prepareOrder(alice, wavesUsdPair, OrderType.SELL, amount, marketPrice, fixedFee)),
                                 tooHighPrice("sell", "0.5"))
    }

    "should be rejected if amount of the buy market order more then user could buy" ignore /* because of TODO: DEX-457 */ {
      val amount   = 101.waves
      val price    = 1.1.usd
      val transfer = 100.usd

      val account = createAccountWithBalance {
        transfer -> Some(UsdId.toString)
      }

      placeOrders(alice, wavesUsdPair, SELL)(
        amount -> price
      )

      assertBadRequestAndMessage(
        node.placeMarketOrder(node.prepareOrder(account, wavesUsdPair, BUY, amount, price, fixedFee)),
        s"Not enough tradable balance. The order requires 0 WAVES and 111.1 ${UsdId}, but available are 111.1 ${UsdId} and 0 WAVES"
      )

    }

    "should be rejected if user has enough balance to fill market order, but has not enough balance to pay fee in another asset" ignore /* because of TODO: DEX-457 */ {
      docker.restartNode(node, ConfigFactory.parseString(s"waves.dex.order-fee.fixed.asset = $BtcId\nwaves.dex.order-fee.mode = fixed"))

      val amount   = 10.waves
      val price    = 1.usd
      val transfer = 10.usd

      val account = createAccountWithBalance { transfer -> Some(UsdId.toString) }

      node.waitOrderStatus(
        wavesUsdPair,
        node.placeOrder(bob, wavesUsdPair, OrderType.SELL, amount, price, fixedFee, feeAsset = IssuedAsset(BtcId), version = 3).message.id,
        "Accepted")

      assertBadRequestAndMessage(
        node.placeMarketOrder(node.prepareOrder(account, wavesUsdPair, BUY, amount, price, fixedFee, feeAsset = IssuedAsset(BtcId))),
        s"Not enough tradable balance. The order requires 0.003 $BtcId and 10 WAVES, but available are 10 WAVES and 0 $BtcId"
      )
    }
  }
}
