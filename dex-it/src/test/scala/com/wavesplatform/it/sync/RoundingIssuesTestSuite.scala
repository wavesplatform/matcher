//package com.wavesplatform.it.sync
//
//import com.wavesplatform.it.MatcherSuiteBase
//import com.wavesplatform.it.api.LevelResponse
//import com.wavesplatform.it.api.SyncHttpApi._
//import com.wavesplatform.it.api.SyncMatcherHttpApi._
//import com.wavesplatform.it.config.DexTestConfig._
//import com.wavesplatform.transaction.assets.exchange.OrderType
//
//import scala.concurrent.duration._
//
//class RoundingIssuesTestSuite extends MatcherSuiteBase {
//  override protected def beforeAll(): Unit = {
//    super.beforeAll()
//    val xs = Seq(IssueUsdTx, IssueEthTx, IssueBtcTx).map(_.json()).map(wavesNode1Api.broadcast(_))
//    xs.foreach(x => wavesNode1Api.waitForTransaction(x.id))
//  }
//
//  "should correctly fill an order with small amount" in {
//    val aliceBalanceBefore = node.accountBalances(alice.toAddress.toString)._1
//    val bobBalanceBefore   = node.accountBalances(bob.toAddress.toString)._1
//
//    val counter   = mkOrder(alice, matcher,wavesUsdPair, OrderType.BUY, 3100000000L, 238)
//    val counterId = dex1Api.place(counter).message.id
//
//    val submitted   = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, 425532L, 235)
//    val submittedId = dex1Api.place(submitted).message.id
//
//    val filledAmount = 420169L
//    node.waitOrderStatusAndAmount(wavesUsdPair, submittedId, "Filled", Some(filledAmount), 1.minute)
//    node.waitOrderStatusAndAmount(wavesUsdPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)
//
//    val tx = node.waitOrderInBlockchain(counterId).head
//    node.cancelOrder(alice, wavesUsdPair, counterId)
//    val rawExchangeTx = node.rawTransactionInfo(tx.id)
//
//    (rawExchangeTx \ "price").as[Long] shouldBe counter.price
//    (rawExchangeTx \ "amount").as[Long] shouldBe filledAmount
//    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
//    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L
//
//    val aliceBalanceAfter = node.accountBalances(alice.toAddress.toString)._1
//    val bobBalanceAfter   = node.accountBalances(bob.toAddress.toString)._1
//
//    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
//    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
//  }
//
//  "reserved balance should not be negative" in {
//    val counter   = mkOrder(bob, matcher,ethBtcPair, OrderType.BUY, 923431000L, 31887L)
//    val counterId = dex1Api.place(counter).message.id
//
//    val submitted   = mkOrder(alice, matcher,ethBtcPair, OrderType.SELL, 223345000L, 31887L)
//    val submittedId = dex1Api.place(submitted).message.id
//
//    val filledAmount = 223344937L
//    node.waitOrderStatusAndAmount(ethBtcPair, submittedId, "Filled", Some(filledAmount), 1.minute)
//    node.waitOrderStatusAndAmount(ethBtcPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)
//
//    withClue("Alice's reserved balance before cancel")(node.reservedBalance(alice) shouldBe empty)
//
//    node.waitOrderInBlockchain(counterId)
//    node.cancelOrder(bob, ethBtcPair, counterId)
//
//    withClue("Bob's reserved balance after cancel")(node.reservedBalance(bob) shouldBe empty)
//  }
//
//  "should correctly fill 2 counter orders" in {
//    val counter1 = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, 98333333L, 60L)
//    dex1Api.place(counter1).message.id
//
//    val counter2   = mkOrder(bob, matcher,wavesUsdPair, OrderType.SELL, 100000000L, 70L)
//    val counter2Id = dex1Api.place(counter2).message.id
//
//    val submitted   = mkOrder(alice, matcher,wavesUsdPair, OrderType.BUY, 100000000L, 1000L)
//    val submittedId = dex1Api.place(submitted).message.id
//
//    node.waitOrderStatusAndAmount(wavesUsdPair, counter2Id, "PartiallyFilled", Some(2857143L), 1.minute)
//    node.waitOrderStatusAndAmount(wavesUsdPair, submittedId, "Filled", Some(99523810L), 1.minute)
//
//    withClue("orderBook check") {
//      val ob = node.orderBook(wavesUsdPair)
//      ob.bids shouldBe empty
//      ob.asks shouldBe List(LevelResponse(97142857L, 70L)) // = 100000000 - 2857143
//    }
//  }
//
//}
