package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.{LevelResponse, OrderStatus, OrderStatusResponse}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration.DurationInt

class MarketOrderTestSuite extends MatcherSuiteBase {

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueEthTx)
  }

  // TODO: test when trying to buy on empty order book?

  "Sunny day tests for market orders" in {

    def placeCounterOrders(sender: KeyPair, pair: AssetPair, ordersType: OrderType)(amountPrices: (Long, Long)*): Unit = {
      val orders = amountPrices.zipWithIndex.map {
        case ((amount, price), i) => mkOrder(sender, pair, ordersType, amount, price, ttl = (i + 1).hours)
      }
      orders.foreach(placeAndAwait(_))
    }

    def placeMarketOrder(sender: KeyPair, pair: AssetPair, orderType: OrderType, amount: Long, price: Long): OrderStatusResponse = {
      val order = mkOrder(sender, pair, orderType, amount, price)
      dex1Api.placeMarket(order)
      dex1Api.waitForOrderStatus(order, OrderStatus.Filled)
    }

    markup("BIG BUY market order executed partially (buy whole counter side):\n")
    placeCounterOrders(alice, ethWavesPair, SELL)(
      1.eth -> 155.20242978.waves,
      2.eth -> 155.20242978.waves,
      3.eth -> 155.08342811.waves
    )

    placeMarketOrder(bob, ethWavesPair, BUY, amount = 10.eth, price = 155.90000000.waves).filledAmount shouldBe Some(6.eth)

    dex1Api.reservedBalance(alice) shouldBe empty
    dex1Api.reservedBalance(bob) shouldBe empty

    dex1Api.orderBook(ethWavesPair).asks shouldBe empty
    dex1Api.orderBook(ethWavesPair).bids shouldBe empty

    markup("SMALL BUY market order executed fully:\n")
    placeCounterOrders(alice, ethWavesPair, SELL)(
      1.eth -> 155.20242978.waves,
      2.eth -> 155.20242978.waves,
      3.eth -> 155.08342811.waves
    )

    placeMarketOrder(bob, ethWavesPair, BUY, amount = 5.eth, price = 155.90000000.waves).filledAmount shouldBe Some(5.eth)

    dex1Api.reservedBalance(alice) shouldBe Map(eth -> 1.eth)
    dex1Api.reservedBalance(bob) shouldBe empty

    dex1Api.orderBook(ethWavesPair).asks shouldBe List { LevelResponse(1.eth, 155.20242978.waves) }
    dex1Api.orderBook(ethWavesPair).bids shouldBe empty

    dex1Api.cancelAll(bob)
    dex1Api.cancelAll(alice)

    markup("BIG SELL market order executed partially (close whole counter side):\n")
    placeCounterOrders(alice, wavesUsdPair, BUY)(
      3.waves -> 1.22.usd,
      2.waves -> 1.21.usd,
      1.waves -> 1.21.usd
    )

    placeMarketOrder(bob, wavesUsdPair, SELL, amount = 10.waves, price = 1.20.usd).filledAmount shouldBe Some(6.waves)

    dex1Api.reservedBalance(alice) shouldBe empty
    dex1Api.reservedBalance(bob) shouldBe empty

    dex1Api.orderBook(wavesUsdPair).asks shouldBe empty
    dex1Api.orderBook(wavesUsdPair).bids shouldBe empty

    markup("SMALL SELL market order executed fully:\n")
    placeCounterOrders(alice, wavesUsdPair, BUY)(
      3.waves -> 1.22.usd,
      2.waves -> 1.21.usd,
      1.waves -> 1.21.usd
    )

    placeMarketOrder(bob, wavesUsdPair, SELL, amount = 5.waves, price = 1.20.usd).filledAmount shouldBe Some(5.waves)

    dex1Api.reservedBalance(alice) shouldBe Map(usd -> 1.21.usd)
    dex1Api.reservedBalance(bob) shouldBe empty

    dex1Api.orderBook(wavesUsdPair).asks shouldBe empty
    dex1Api.orderBook(wavesUsdPair).bids shouldBe List { LevelResponse(1.waves, 1.21.usd) }
  }

  "Market order should be executed correctly when available for spending < required by spendable asset" in {
    pending
  }
}
