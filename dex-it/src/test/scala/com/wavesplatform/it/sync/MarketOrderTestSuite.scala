package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.model.MatcherModel.Normalization
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.api.{LevelResponse, MatcherStatusResponse}
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}
import mouse.any._

class MarketOrderTestSuite extends MatcherSuiteBase {

  implicit class DoubleOps(value: Double) {
    val waves, eth: Long = Normalization.normalizeAmountAndFee(value, 8)
    val usd: Long        = Normalization.normalizePrice(value, 8, 2)
  }

  override protected def nodeConfigs: Seq[Config] = {
    super.nodeConfigs.map(ConfigFactory.parseString("waves.dex.allowed-order-versions = [1, 2, 3]").withFallback)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Seq(IssueUsdTx, IssueEthTx) foreach { tx =>
      node.waitForTransaction(node.broadcastRequest(tx.json.value).id)
    }
  }

  "Sunny day tests for market orders" in {

    def placeCounterOrders(sender: KeyPair, pair: AssetPair, ordersType: OrderType)(orders: (Long, Long)*): Unit = {
      orders.foreach {
        case (amount, price) =>
          node.placeOrder(sender, pair, ordersType, amount, price, 0.003.waves).message.id |> (lo => node.waitOrderStatus(pair, lo, "Accepted"))
      }
    }

    def placeMarketOrder(sender: KeyPair, pair: AssetPair, orderType: OrderType, amount: Long, price: Long): MatcherStatusResponse = {
      node.prepareOrder(sender, pair, orderType, amount, price, fee = 0.003.waves) |>
        (markerOrder => node.placeMarketOrder(markerOrder).message.id) |>
        (orderId => node.waitOrderStatus(pair, orderId, "Filled"))
    }

    withClue("BIG BUY market order executed partially (buy whole counter side):\n") {
      placeCounterOrders(alice, ethWavesPair, SELL)(
        1.eth -> 155.20242978.waves,
        2.eth -> 155.20242978.waves,
        3.eth -> 155.08342811.waves
      )

      placeMarketOrder(bob, ethWavesPair, BUY, amount = 10.eth, price = 155.90000000.waves).filledAmount shouldBe Some(6.eth)

      node.reservedBalance(alice) shouldBe empty
      node.reservedBalance(bob) shouldBe empty

      node.orderBook(ethWavesPair).asks shouldBe empty
      node.orderBook(ethWavesPair).bids shouldBe empty
    }

    withClue("SMALL BUY market order executed fully:\n") {
      placeCounterOrders(alice, ethWavesPair, SELL)(
        1.eth -> 155.20242978.waves,
        2.eth -> 155.20242978.waves,
        3.eth -> 155.08342811.waves
      )

      placeMarketOrder(bob, ethWavesPair, BUY, amount = 5.eth, price = 155.90000000.waves).filledAmount shouldBe Some(5.eth)

      node.reservedBalance(alice) shouldBe Map { EthId.toString -> 1.eth }
      node.reservedBalance(bob) shouldBe empty

      node.orderBook(ethWavesPair).asks shouldBe List { LevelResponse(1.eth, 155.20242978.waves) }
      node.orderBook(ethWavesPair).bids shouldBe empty
    }

    node <| { _.cancelAllOrders(bob) } <| { _.cancelAllOrders(alice) }

    withClue("BIG SELL market order executed partially (close whole counter side):\n") {
      placeCounterOrders(alice, wavesUsdPair, BUY)(
        3.waves -> 1.22.usd,
        2.waves -> 1.21.usd,
        1.waves -> 1.21.usd
      )

      placeMarketOrder(bob, wavesUsdPair, SELL, amount = 10.waves, price = 1.20.usd).filledAmount shouldBe Some(6.waves)

      node.reservedBalance(alice) shouldBe empty
      node.reservedBalance(bob) shouldBe empty

      node.orderBook(wavesUsdPair).asks shouldBe empty
      node.orderBook(wavesUsdPair).bids shouldBe empty
    }

    withClue("SMALL SELL market order executed fully:\n") {
      placeCounterOrders(alice, wavesUsdPair, BUY)(
        3.waves -> 1.22.usd,
        2.waves -> 1.21.usd,
        1.waves -> 1.21.usd
      )

      placeMarketOrder(bob, wavesUsdPair, SELL, amount = 5.waves, price = 1.20.usd).filledAmount shouldBe Some(5.waves)

      node.reservedBalance(alice) shouldBe Map { UsdId.toString -> 1.21.usd }
      node.reservedBalance(bob) shouldBe empty

      node.orderBook(wavesUsdPair).asks shouldBe empty
      node.orderBook(wavesUsdPair).bids shouldBe List { LevelResponse(1.waves, 1.21.usd) }
    }
  }

  "Market order should be executed correctly when available for spending < required by spendable asset" in {
    pending
  }
}
