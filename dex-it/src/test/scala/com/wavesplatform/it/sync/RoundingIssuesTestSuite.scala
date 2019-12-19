package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.responses.dex.{LevelResponse, OrderStatus, OrderStatusResponse}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, OrderType}

class RoundingIssuesTestSuite extends MatcherSuiteBase {

  override protected def suiteInitialDexConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES" ]""")

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNode1Container(), wavesNode1Api)
    broadcastAndAwait(IssueUsdTx, IssueEthTx, IssueBtcTx)
    startAndWait(dex1Container(), dex1Api)
  }

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = wavesNode1Api.balance(alice, Waves)
    val bobBalanceBefore   = wavesNode1Api.balance(bob, Waves)

    val counter = mkOrder(alice, wavesUsdPair, OrderType.BUY, 3100000000L, 238)
    dex1Api.place(counter)

    val submitted = mkOrder(bob, wavesUsdPair, OrderType.SELL, 425532L, 235)
    dex1Api.place(submitted)

    val filledAmount = 420169L
    dex1Api.waitForOrder(submitted)(_ == OrderStatusResponse(OrderStatus.Filled, Some(filledAmount)))
    dex1Api.waitForOrder(counter)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, Some(filledAmount)))

    val tx = waitForOrderAtNode(counter)
    dex1Api.cancel(alice, counter)

    val exchangeTx = wavesNode1Api.transactionInfo(tx.id()).getOrElse(throw new RuntimeException(s"Can't find tx with id = '${tx.id()}'")) match {
      case r: ExchangeTransaction => r
      case x                      => throw new RuntimeException(s"Expected ExchangeTransaction, but got $x")
    }

    exchangeTx.price shouldBe counter.price
    exchangeTx.amount shouldBe filledAmount
    exchangeTx.buyMatcherFee shouldBe 40L
    exchangeTx.sellMatcherFee shouldBe 296219L

    val aliceBalanceAfter = wavesNode1Api.balance(alice, Waves)
    val bobBalanceAfter   = wavesNode1Api.balance(bob, Waves)

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

  "reserved balance should not be negative" in {
    val counter = mkOrder(bob, ethBtcPair, OrderType.BUY, 923431000L, 31887L)
    dex1Api.place(counter)

    val submitted = mkOrder(alice, ethBtcPair, OrderType.SELL, 223345000L, 31887L)
    dex1Api.place(submitted)

    val filledAmount = 223344937L
    dex1Api.waitForOrder(submitted)(_ == OrderStatusResponse(OrderStatus.Filled, Some(filledAmount)))
    dex1Api.waitForOrder(counter)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, Some(filledAmount)))

    withClue("Alice's reserved balance before cancel")(dex1Api.reservedBalance(alice) shouldBe empty)

    waitForOrderAtNode(counter)
    dex1Api.cancel(bob, counter)

    withClue("Bob's reserved balance after cancel")(dex1Api.reservedBalance(bob) shouldBe empty)
  }

  "should correctly fill 2 counter orders" in {
    val counter1 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 98333333L, 60L)
    dex1Api.place(counter1)

    val counter2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 100000000L, 70L)
    dex1Api.place(counter2)

    val submitted = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100000000L, 1000L)
    dex1Api.place(submitted)

    dex1Api.waitForOrder(submitted)(_ == OrderStatusResponse(OrderStatus.Filled, Some(99523810L)))
    dex1Api.waitForOrder(counter2)(_ == OrderStatusResponse(OrderStatus.PartiallyFilled, Some(2857143L)))

    withClue("orderBook check") {
      val ob = dex1Api.orderBook(wavesUsdPair)
      ob.bids shouldBe empty
      ob.asks shouldBe List(LevelResponse(97142857L, 70L)) // = 100000000 - 2857143
    }
  }
}
