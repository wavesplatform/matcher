package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

class MarketOrderSuite extends MatcherSuiteBase {

  val fee = 0.003.waves

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
         |
         |  allowed-order-versions = [1, 2, 3]
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
    node.waitForTransaction(node.broadcastRequest(IssueUsdTx.json()).id)
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    node.cancelOrdersForPair(alice, wavesUsdPair)
    node.cancelOrdersForPair(bob, wavesUsdPair)
  }

  def placeOrders(sender: KeyPair, pair: AssetPair, orderType: OrderType)(orders: (Long, Long)*): Unit = {
    orders.foreach {
      case (amount, price) =>
        val order = node.placeOrder(sender = sender, pair = pair, orderType = orderType, amount = amount, price = price, fee = fee)
        node.waitOrderStatus(pair, order.message.id, OrderStatus.Accepted.name)
    }
  }

  def printBalances(account: KeyPair): Unit = {
    System.out.println(account.toString)
    System.out.println(s"waves: ${node.accountBalances(account.toAddress.toString)._1} ")
    System.out.println(s"usd: ${node.assetBalance(account.toAddress.toString, UsdId.toString).balance} ")
    System.out.println(s"waves-r: ${node.reservedBalance(account).getOrElse("WAVES", "0")} ")
    System.out.println(s"usd-r: ${node.reservedBalance(account).getOrElse(UsdId.toString, "0")} ")
  }

  "Processing market orders" - {
    val price = 0.4.usd

    "processing market order (SELL)" in {
      val amount       = 10000.waves
      val aliceWBefore = node.accountBalances(alice.toAddress.toString)._1
      val bobWBefore   = node.accountBalances(bob.toAddress.toString)._1
      val aliceUBefore = node.assetBalance(alice.toAddress.toString, UsdId.toString).balance
      val bobUBefore   = node.assetBalance(bob.toAddress.toString, UsdId.toString).balance

      placeOrders(alice, wavesUsdPair, OrderType.BUY)(amount -> price)

      val marketOrder = node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, OrderType.SELL, amount, price, fee))
      node.waitOrderProcessed(wavesUsdPair, marketOrder.message.id)
      node.waitOrderStatusAndAmount(wavesUsdPair, marketOrder.message.id, "Filled", Some(amount))

      node.accountBalances(alice.toAddress.toString)._1 should be(aliceWBefore + amount - fee)
      node.assetBalance(alice.toAddress.toString, UsdId.toString).balance should be(aliceUBefore - price * amount / 1.waves)

      node.accountBalances(bob.toAddress.toString)._1 should be(bobWBefore - amount - fee)
      node.assetBalance(bob.toAddress.toString, UsdId.toString).balance should be(bobUBefore + price * amount / 1.waves)
    }

    "processing market order (BUY)" in {
      val amount       = 5.waves
      val aliceWBefore = node.accountBalances(alice.toAddress.toString)._1
      val bobWBefore   = node.accountBalances(bob.toAddress.toString)._1
      val aliceUBefore = node.assetBalance(alice.toAddress.toString, UsdId.toString).balance
      val bobUBefore   = node.assetBalance(bob.toAddress.toString, UsdId.toString).balance

      placeOrders(alice, wavesUsdPair, OrderType.SELL)(amount -> price)

      node.waitOrderProcessed(wavesUsdPair, node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, OrderType.BUY, amount, price, fee)).message.id)

      node.accountBalances(alice.toAddress.toString)._1 should be(aliceWBefore - amount - fee)
      node.assetBalance(alice.toAddress.toString, UsdId.toString).balance should be(aliceUBefore + price * amount / 1.waves)

      node.accountBalances(bob.toAddress.toString)._1 should be(bobWBefore + amount - fee)
      node.assetBalance(bob.toAddress.toString, UsdId.toString).balance should be(bobUBefore - price * amount / 1.waves)
    }

    "should be matched with order having the best price (BUY)" in {
      val marketPrice = 0.4.usd
      val amount      = 25.waves
      val bestPrice   = 0.3.usd

      placeOrders(alice, wavesUsdPair, OrderType.SELL)(
        50.waves -> 0.33.usd,
        10.waves -> bestPrice,
        25.waves -> 0.35.usd,
      )

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, OrderType.BUY, amount, marketPrice, fee)).message.id)

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
        50.waves -> 0.78.usd,
        10.waves -> bestPrice,
        25.waves -> secondPrice,
      )

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeMarketOrder(node.prepareOrder(bob, wavesUsdPair, OrderType.SELL, amount, marketPrice, fee)).message.id)

      val orderBook = node.orderBook(wavesUsdPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks should be(empty)

      orderBook.bids.filter(order => order.price == bestPrice) should be(empty)
      orderBook.bids.filter(order => order.price == secondPrice && order.amount == 10.waves) should have size 1
      orderBook.bids should have size 2
    }

    "should be removed from order book when the restriction by tokens count has been reached (BUY)" in {}

    "should be removed from order book when the restriction by tokens count has been reached (SELL)" in {}

    "should be removed from order book when the user has not enough spendable balance (BUY)" in {}

    "should be removed from order book when the user has not enough spendable balance (SELL)" in {}

    "should be removed from order book when there are no suitable orders by limit of the price (BUY)" in {}

    "should be removed from order book when there are no suitable orders by limit of the price (SELL)" in {}

  }

  "Validation market orders" - {
    val price = 0.4.usd

    "should be accepted if there is no way to fill it completely (sum of all orders in order book < amount of market order" in {
      placeOrders(bob, wavesUsdPair, OrderType.SELL)(100.waves -> price)

      var total = 0L
      node
        .orderBook(wavesUsdPair)
        .bids
        .foreach(order => {
          total += order.amount
        })

      node.waitOrderProcessed(wavesUsdPair,
                              node.placeMarketOrder(node.prepareOrder(alice, wavesUsdPair, OrderType.BUY, total + 1000.waves, price, fee)).message.id)

    }

    "should be rejected if the price is too high for completely filling by current opened orders (BUY)" in {
      val amount      = 100.waves
      val marketPrice = price - 0.1.usd

      placeOrders(bob, wavesUsdPair, OrderType.SELL)(amount -> price)

      assertBadRequestAndMessage(node.placeMarketOrder(node.prepareOrder(alice, wavesUsdPair, OrderType.BUY, amount, marketPrice, fee)),
                                 tooLowPrice("buy", "0.3"))

    }

    "should be rejected if the price is too high for completely filling by current opened orders (SELL)" in {
      val amount      = 100.waves
      val marketPrice = price + 0.1.usd

      placeOrders(bob, wavesUsdPair, OrderType.BUY)(amount -> price)

      assertBadRequestAndMessage(node.placeMarketOrder(node.prepareOrder(alice, wavesUsdPair, OrderType.SELL, amount, marketPrice, fee)),
                                 tooHighPrice("sell", "0.5"))

    }

  }
}
