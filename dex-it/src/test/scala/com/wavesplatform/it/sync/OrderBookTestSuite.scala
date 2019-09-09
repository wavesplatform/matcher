package com.wavesplatform.it.sync

import com.wavesplatform.account.KeyPair
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType._

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class OrderBookTestSuite extends NewMatcherSuiteBase {

  private case class ReservedBalances(wct: Long, usd: Long, waves: Long)
  private def reservedBalancesOf(pk: KeyPair): ReservedBalances = {
    val reservedBalances = dex1Api.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(wct, 0),
      reservedBalances.getOrElse(usd, 0),
      reservedBalances.getOrElse(Waves, 0)
    )
  }

  private val (amount, price)         = (1000L, PriceConstant)
  private val buyOrder                = mkOrder(alice, wctUsdPair, BUY, 2 * amount, price)
  private val anotherBuyOrder         = mkOrder(alice, wctUsdPair, BUY, amount, price)
  private val sellOrder               = mkOrder(bob, wctUsdPair, SELL, amount, 2 * price)
  private val buyOrderForAnotherPair  = mkOrder(alice, wctWavesPair, BUY, amount, price)
  private val sellOrderForAnotherPair = mkOrder(bob, wctWavesPair, SELL, amount, 2 * price)

  private var aliceRBForOnePair = ReservedBalances(0, 0, 0)
  private var bobRBForOnePair   = ReservedBalances(0, 0, 0)

  private var aliceRBForBothPairs = ReservedBalances(0, 0, 0)
  private var bobRBForBothPairs   = ReservedBalances(0, 0, 0)

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueWctTx)
  }

  "Place orders and delete the order book" in {
    val submitted = mkOrder(bob, wctUsdPair, SELL, amount, price)
    List(buyOrder, anotherBuyOrder, submitted, sellOrder).foreach(dex1Api.place)

    dex1Api.waitForOrderStatus(buyOrder, OrderStatus.PartiallyFilled)
    dex1Api.waitForOrderStatus(submitted, OrderStatus.Filled)

    aliceRBForOnePair = reservedBalancesOf(alice)
    bobRBForOnePair = reservedBalancesOf(bob)

    List(buyOrderForAnotherPair, sellOrderForAnotherPair).foreach(dex1Api.place)

    dex1Api.waitForOrderStatus(buyOrderForAnotherPair, OrderStatus.Accepted)
    dex1Api.waitForOrderStatus(sellOrderForAnotherPair, OrderStatus.Accepted)

    aliceRBForBothPairs = reservedBalancesOf(alice)
    bobRBForBothPairs = reservedBalancesOf(bob)

    dex1Api.tryDeleteOrderBook(wctUsdPair) shouldBe 'right
  }

  "When delete order book" - {
    "orders by the pair should be canceled" in {
      dex1Api.waitForOrderStatus(buyOrder, OrderStatus.Cancelled)
      dex1Api.waitForOrderStatus(anotherBuyOrder, OrderStatus.Cancelled)
      dex1Api.waitForOrderStatus(sellOrder, OrderStatus.Cancelled)
    }

    "the order book was deleted" in {
      withClue("orderBook") {
        val orderBook = dex1Api.orderBook(wctUsdPair)
        orderBook.bids shouldBe empty
        orderBook.asks shouldBe empty
      }

      withClue("tradingMarkets") {
        val tradingPairs = dex1Api.allOrderBooks.markets.map(x => s"${x.amountAsset}-${x.priceAsset}")
        tradingPairs shouldNot contain(wctUsdPair.key)
      }

      withClue("getAllSnapshotOffsets") {
        dex1Api.allSnapshotOffsets.keySet shouldNot contain(wctUsdPair.key)
      }
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.waves shouldBe (aliceRBForBothPairs.waves - aliceRBForOnePair.waves)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.waves shouldBe (bobRBForBothPairs.waves - bobRBForOnePair.waves)
    }

    "it should not affect other pairs and their orders" in {
      dex1Api.orderStatus(buyOrderForAnotherPair).status shouldBe OrderStatus.Accepted
      dex1Api.orderStatus(sellOrderForAnotherPair).status shouldBe OrderStatus.Accepted
      dex1Api.place(mkOrder(alice, wctWavesPair, BUY, amount, price))

      val orderBook = dex1Api.orderBook(wctWavesPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "matcher can start after multiple delete events" in {
      def deleteWctWaves = dex1AsyncApi.tryDeleteOrderBook(wctWavesPair)
      val deleteMultipleTimes = deleteWctWaves
        .zip(deleteWctWaves)
        .map(_ => ())
        .recover { case _ => () } // It's ok: either this should fail, or restartNode should work

      Await.ready(deleteMultipleTimes, 1.minute)
      restartContainer(dex1Container(), dex1Api)
    }
  }
}
