package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.responses.dex.{LevelResponse, OrderStatus, OrderStatusResponse}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.wavesj.transactions.ExchangeTransaction

class RoundingIssuesTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueEthTx, IssueBtcTx)
    dex1.start()
  }

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = wavesNode1.api.balance(alice, Waves)
    val bobBalanceBefore   = wavesNode1.api.balance(bob, Waves)

    val counter = mkOrder(alice, wavesUsdPair, OrderType.BUY, 3100000000L, 238)
    dex1.api.place(counter)

    val submitted = mkOrder(bob, wavesUsdPair, OrderType.SELL, 425532L, 235)
    dex1.api.place(submitted)

    val filledAmount = 420169L
    dex1.api.waitForOrder(submitted)(_ == OrderStatusResponse(OrderStatus.Filled, Some(filledAmount), Some(296219L)))
    dex1.api.waitForOrder(counter)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, Some(filledAmount), Some(40L)))

    val tx = waitForOrderAtNode(counter)
    dex1.api.cancel(alice, counter)

    val exchangeTx = wavesNode1.api.transactionInfo(tx.head.getId).getOrElse(throw new RuntimeException(s"Can't find tx with id = '${tx.head.getId}'")) match {
      case r: ExchangeTransaction => r
      case x                      => throw new RuntimeException(s"Expected ExchangeTransaction, but got $x")
    }

    exchangeTx.getPrice shouldBe counter.price
    exchangeTx.getAmount shouldBe filledAmount
    exchangeTx.getBuyMatcherFee shouldBe 40L
    exchangeTx.getSellMatcherFee shouldBe 296219L

    val aliceBalanceAfter = wavesNode1.api.balance(alice, Waves)
    val bobBalanceAfter   = wavesNode1.api.balance(bob, Waves)

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

  "reserved balance should not be negative" in {
    val counter = mkOrder(bob, ethBtcPair, OrderType.BUY, 923431000L, 31887L)
    dex1.api.place(counter)

    val submitted = mkOrder(alice, ethBtcPair, OrderType.SELL, 223345000L, 31887L)
    dex1.api.place(submitted)

    val filledAmount = 223344937L
    dex1.api.waitForOrder(submitted)(_ == OrderStatusResponse(OrderStatus.Filled, filledAmount = Some(filledAmount), filledFee = Some(299999L)))
    dex1.api.waitForOrder(counter)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, filledAmount = Some(filledAmount), filledFee = Some(72559L)))

    withClue("Alice's reserved balance before cancel")(dex1.api.reservedBalance(alice) shouldBe empty)

    waitForOrderAtNode(counter)
    dex1.api.cancel(bob, counter)

    withClue("Bob's reserved balance after cancel")(dex1.api.reservedBalance(bob) shouldBe empty)
  }

  "should correctly fill 2 counter orders" in {
    val counter1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 98333333L, 60L)
    dex1.api.place(counter1)

    val counter2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100000000L, 70L)
    dex1.api.place(counter2)

    val submitted = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100000000L, 1000L)
    dex1.api.place(submitted)

    dex1.api.waitForOrder(submitted)(_ == OrderStatusResponse(OrderStatus.Filled, filledAmount = Some(99523810L), filledFee = Some(298571L)))
    dex1.api.waitForOrder(counter2)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, filledAmount = Some(2857143L), filledFee = Some(8571L)))

    withClue("orderBook check") {
      val ob = dex1.api.orderBook(wavesUsdPair)
      ob.bids shouldBe empty
      ob.asks shouldBe List(LevelResponse(97142857L, 70L)) // = 100000000 - 2857143
    }
  }
}
