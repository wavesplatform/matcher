package com.wavesplatform.it.sync

import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.{LevelResponse, OrderStatus, OrderStatusResponse}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.OrderType

class RoundingIssuesTestSuite extends NewMatcherSuiteBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx, IssueEthTx, IssueBtcTx)
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

    val tx = waitForOrderAtNode(counter.id())
    dex1Api.cancel(alice, counter)
    val rawExchangeTx = wavesNode1Api.rawTransactionInfo(tx.id()).getOrElse(throw new RuntimeException(s"Can't find tx with id = '${tx.id()}'"))

    (rawExchangeTx \ "price").as[Long] shouldBe counter.price
    (rawExchangeTx \ "amount").as[Long] shouldBe filledAmount
    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L

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

    waitForOrderAtNode(counter.id())
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
