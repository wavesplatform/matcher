package com.wavesplatform.dex.market

import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorRef
import akka.testkit.{ImplicitSender, TestActorRef, TestProbe}
import cats.data.NonEmptyList
import com.wavesplatform.NTPTime
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.MatcherTestData
import com.wavesplatform.dex.api.AlreadyProcessed
import com.wavesplatform.dex.db.OrderBookSnapshotDB
import com.wavesplatform.dex.fixtures.RestartableActor
import com.wavesplatform.dex.fixtures.RestartableActor.RestartActor
import com.wavesplatform.dex.market.MatcherActor.SaveSnapshot
import com.wavesplatform.dex.market.OrderBookActor._
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent.Canceled
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatchingRule}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderOps._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._

class OrderBookActorSpecification
    extends MatcherSpec("OrderBookActor")
    with NTPTime
    with ImplicitSender
    with MatcherTestData
    with PathMockFactory
    with Eventually {

  private val obc = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot]
  private val md  = new ConcurrentHashMap[AssetPair, MarketStatus]

  private val wctAsset = IssuedAsset(ByteStr(Array.fill(32)(1)))
  private val ethAsset = IssuedAsset(ByteStr("ETH".getBytes))

  private def update(ap: AssetPair)(snapshot: OrderBook.AggregatedSnapshot): Unit = obc.put(ap, snapshot)

  private def obcTest(f: (AssetPair, TestActorRef[OrderBookActor with RestartableActor], TestProbe) => Unit): Unit =
    obcTestWithPrepare((_, _) => ()) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithTickSize(tickSize: Double)(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit =
    obcTestWithPrepare((_, _) => (), NonEmptyList(DenormalizedMatchingRule(0L, tickSize), List.empty)) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithMatchingRules(matchingRules: NonEmptyList[DenormalizedMatchingRule])(f: (AssetPair, ActorRef, TestProbe) => Unit): Unit =
    obcTestWithPrepare((_, _) => (), matchingRules) { (pair, actor, probe) =>
      probe.expectMsg(OrderBookRecovered(pair, None))
      f(pair, actor, probe)
    }

  private def obcTestWithPrepare(prepare: (OrderBookSnapshotDB, AssetPair) => Unit,
                                 matchingRules: NonEmptyList[DenormalizedMatchingRule] = NonEmptyList.one(DenormalizedMatchingRule(0, 0.00000001)))(
      f: (AssetPair, TestActorRef[OrderBookActor with RestartableActor], TestProbe) => Unit): Unit = {

    obc.clear()
    md.clear()

    val tp    = TestProbe()
    val pair  = AssetPair(wctAsset, Waves)
    val obsdb = OrderBookSnapshotDB.inMem

    prepare(obsdb, pair)

    val orderBookActor = TestActorRef(
      new OrderBookActor(
        tp.ref,
        tp.ref,
        system.actorOf(OrderBookSnapshotStoreActor.props(obsdb)),
        pair,
        update(pair),
        p => Option(md.get(p)),
        ntpTime,
        matchingRules,
        _ => (),
        raw => MatchingRule(raw.startOffset, (BigDecimal(raw.tickSize) * BigDecimal(10).pow(8)).toLongExact)
      ) with RestartableActor)

    f(pair, orderBookActor, tp)
    system.stop(orderBookActor)
  }

  "OrderBookActor" should {
    "recover from snapshot - 1" in obcTestWithPrepare((_, _) => ()) { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, None))
    }

    "recover from snapshot - 2" in obcTestWithPrepare { (obsdb, p) =>
      obsdb.update(p, 50, Some(OrderBook.empty.snapshot))
    } { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "recovery - notify address actor about orders" in obcTestWithPrepare(
      { (obsdb, p) =>
        val ord = buy(p, 10 * Order.PriceConstant, 100)
        val ob  = OrderBook.empty
        ob.add(LimitOrder(ord), ord.timestamp)
        obsdb.update(p, 50, Some(ob.snapshot))
      }
    ) { (pair, _, tp) =>
      tp.expectMsgType[OrderAdded]
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "place buy and sell order to the order book and preserve it after restart" in obcTest { (pair, orderBook, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 150)

      orderBook ! wrapLimitOrder(ord1)
      orderBook ! wrapLimitOrder(ord2)
      tp.receiveN(2)

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      orderBook ! RestartActor

      tp.receiveN(2) shouldEqual Seq(ord1, ord2).map(o => OrderAdded(LimitOrder(o), o.timestamp))
      tp.expectMsgType[OrderBookRecovered]
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      actor ! wrapLimitOrder(ord2)

      tp.receiveN(3)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]

      actor ! RestartActor
      tp.expectMsg(
        OrderAdded(SellLimitOrder(
                     ord2.amount - ord1.amount,
                     ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord1.amount),
                     ord2
                   ),
                   ord2.timestamp)
      )
      tp.expectMsgType[OrderBookRecovered]
    }

    "execute one order fully and other partially and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = buy(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 12 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      actor ! wrapLimitOrder(ord2)
      actor ! wrapLimitOrder(ord3)
      tp.receiveN(4)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      tp.expectMsg(
        OrderAdded(BuyLimitOrder(
                     restAmount,
                     ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
                     ord2
                   ),
                   ord2.timestamp)
      )
      tp.expectMsgType[OrderBookRecovered]
    }

    "match multiple best orders at once and restore after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 5 * Order.PriceConstant, 100)
      val ord3 = sell(pair, 5 * Order.PriceConstant, 90)
      val ord4 = buy(pair, 19 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      actor ! wrapLimitOrder(ord2)
      actor ! wrapLimitOrder(ord3)
      actor ! wrapLimitOrder(ord4)
      tp.receiveN(6)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      tp.expectMsg(
        OrderAdded(
          SellLimitOrder(
            restAmount,
            ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2
          ),
          ord2.timestamp
        ))
      tp.expectMsgType[OrderBookRecovered]
    }

    "place orders and restart without waiting for response" in obcTest { (pair, orderBook, tp) =>
      val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
      val ts   = System.currentTimeMillis()

      (1 to 100) foreach { i =>
        orderBook ! wrapLimitOrder(ord1.updateTimestamp(ts + i))
      }

      within(10.seconds) {
        tp.receiveN(100)
      }

      orderBook ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      orderBook ! RestartActor

      within(10.seconds) {
        tp.receiveN(100)
      }
      tp.expectMsgType[OrderBookRecovered]
    }

    "ignore outdated requests" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      all(receiveN(10)) shouldBe AlreadyProcessed
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsg(OrderBookSnapshotUpdateCompleted(pair, Some(10)))

      (11 to 20).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(20)
      tp.expectMsg(OrderBookSnapshotUpdateCompleted(pair, Some(20)))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      tp.expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
    }

    "cancel order with price not equal to it's level price" in obcTestWithTickSize(100) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)

      orderBook ! wrapLimitOrder(1, buyOrder)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapEvent(2, Canceled(buyOrder.assetPair, buyOrder.id()))
      tp.expectMsgType[OrderCanceled]
    }

    val switchRulesTest = NonEmptyList(
      DenormalizedMatchingRule(0, 0.00000001),
      List(
        DenormalizedMatchingRule(4, 0.000001),
        DenormalizedMatchingRule(10, 0.000003)
      )
    )

    "rules are switched" in obcTestWithMatchingRules(switchRulesTest) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)
      (0 to 17).foreach { i =>
        orderBook ! wrapLimitOrder(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 3

        val level41 = bids.head
        level41.price shouldBe buyOrder.price
        level41.amount shouldBe buyOrder.amount * 4

        val level40 = bids(1)
        level40.price shouldBe (0.000004 * Order.PriceConstant)
        level40.amount shouldBe buyOrder.amount * 6

        val level30 = bids(2)
        level30.price shouldBe (0.000003 * Order.PriceConstant)
        level30.amount shouldBe buyOrder.amount * 8
      }
    }

    val disableRulesTest = NonEmptyList(
      DenormalizedMatchingRule(0, 0.000001),
      List(
        DenormalizedMatchingRule(3, 0.00000001)
      )
    )

    "rules can be disabled" in obcTestWithMatchingRules(disableRulesTest) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000, 0.0000041)
      (0 to 10).foreach { i =>
        orderBook ! wrapLimitOrder(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2

        val level41 = bids.head
        level41.price shouldBe buyOrder.price
        level41.amount shouldBe buyOrder.amount * 8

        val level40 = bids(1)
        level40.price shouldBe (0.000004 * Order.PriceConstant)
        level40.amount shouldBe buyOrder.amount * 3
      }
    }

    val matchingRulesForCancelTest = NonEmptyList(
      DenormalizedMatchingRule(0, 0.00000001),
      List(
        DenormalizedMatchingRule(0, 0.00000001),
        DenormalizedMatchingRule(0, 0.000001)
      )
    )

    "correctly cancel order when rules are switched" in obcTestWithMatchingRules(matchingRulesForCancelTest) { (pair, orderBook, tp) =>
      val buyOrder1, buyOrder2 = buy(pair, 100000000, 0.0000041)

      orderBook ! wrapLimitOrder(0, buyOrder1) // order book places order to the price level 41
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapLimitOrder(1, buyOrder2) // now order book places the same order to the price level 40
      tp.expectMsgType[OrderAdded]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 2
        bids.head.price shouldBe buyOrder1.price
        bids.last.price shouldBe 0.0000040 * Order.PriceConstant
      }

      orderBook ! wrapEvent(2, Canceled(buyOrder1.assetPair, buyOrder1.id())) // order book is looking for the price level of buyOrder1 correctly (41 but not 40)
      tp.expectMsgType[OrderCanceled]

      eventually {
        val bids = obc.get(pair).bids
        bids.size shouldBe 1
        bids.head.price shouldBe 0.000004 * Order.PriceConstant
      }
    }

    "correctly handle big market orders" in {

      def bigMarketOrderTest(marketOrderType: OrderType, feeAsset: Asset): Unit = obcTest { (wctWavesPair, orderBook, tp) =>
        val counterOrder1Amount = toNormalized(12) // will be executed fully
        val counterOrder2Amount = toNormalized(5)  // will be executed fully
        val counterOrder3Amount = toNormalized(3)  // will be executed partially

        val marketOrderAmount = toNormalized(18)

        val (counterOrder1, counterOrder2, counterOrder3, marketOrder) = marketOrderType match {
          case OrderType.BUY =>
            val buyOrder       = buy(wctWavesPair, amount = marketOrderAmount, price = 110, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketBuyOrder = MarketOrder(buyOrder, availableForSpending = getSpentAmountWithFee(buyOrder))
            (
              sell(wctWavesPair, amount = counterOrder1Amount, price = 105),
              sell(wctWavesPair, amount = counterOrder2Amount, price = 110),
              sell(wctWavesPair, amount = counterOrder3Amount, price = 110),
              marketBuyOrder
            )
          case OrderType.SELL =>
            val sellOrder       = sell(wctWavesPair, amount = marketOrderAmount, price = 95, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketSellOrder = MarketOrder(sellOrder, availableForSpending = getSpentAmountWithFee(sellOrder))
            (
              buy(wctWavesPair, amount = counterOrder1Amount, price = 110),
              buy(wctWavesPair, amount = counterOrder2Amount, price = 105),
              buy(wctWavesPair, amount = counterOrder3Amount, price = 105),
              marketSellOrder
            )
        }

        Seq(counterOrder1, counterOrder2, counterOrder3).foreach { o =>
          orderBook ! wrapLimitOrder(o)
          tp.expectMsgType[OrderAdded]
        }

        orderBook ! wrapMarketOrder(marketOrder)

        val oe1 = tp.expectMsgType[OrderExecuted]
        oe1.submitted shouldBe marketOrder
        oe1.counter shouldBe LimitOrder(counterOrder1)
        oe1.executedAmount shouldBe counterOrder1.amount

        val oe2                   = tp.expectMsgType[OrderExecuted]
        val marketOrderRemaining1 = oe1.submittedMarketRemaining(marketOrder)
        oe2.submitted shouldBe marketOrderRemaining1
        oe2.counter shouldBe LimitOrder(counterOrder2)
        oe2.executedAmount shouldBe counterOrder2.amount

        val oe3                   = tp.expectMsgType[OrderExecuted]
        val marketOrderRemaining2 = oe2.submittedMarketRemaining(marketOrderRemaining1)
        oe3.submitted shouldBe marketOrderRemaining2
        oe3.counter shouldBe LimitOrder(counterOrder3)
        oe3.executedAmount shouldBe toNormalized(1)

        tp.receiveN(0)

        eventually {
          obc.get(wctWavesPair).getCounterSideFor(marketOrder).map(_.amount).sum shouldBe toNormalized(2)
          obc.get(wctWavesPair).getSideFor(marketOrder) shouldBe empty
        }
      }

      bigMarketOrderTest(OrderType.SELL, feeAsset = Waves)    // fee in received asset
      bigMarketOrderTest(OrderType.SELL, feeAsset = ethAsset) // fee in third asset
      bigMarketOrderTest(OrderType.SELL, feeAsset = wctAsset) // fee in spent asset

      bigMarketOrderTest(OrderType.BUY, feeAsset = wctAsset) // fee in received asset
      bigMarketOrderTest(OrderType.BUY, feeAsset = ethAsset) // fee in third asset
      bigMarketOrderTest(OrderType.BUY, feeAsset = Waves)    // fee in spent asset
    }

    "cancel market orders because of the stop conditions (no counter orders or partially filled)" in {

      def noCountersOrPartiallyFilledTest(marketOrderType: OrderType, feeAsset: Asset): Unit = obcTest { (wctWavesPair, orderBook, tp) =>
        val marketOrderAmount  = toNormalized(10)
        val counterOrderAmount = toNormalized(4)

        val (counterOrder, marketOrder) = marketOrderType match {
          case OrderType.SELL =>
            val sellOrder       = sell(wctWavesPair, amount = marketOrderAmount, price = 90, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketSellOrder = MarketOrder(sellOrder, availableForSpending = getSpentAmountWithFee(sellOrder))
            (
              buy(wctWavesPair, amount = counterOrderAmount, price = 100, matcherFee = smallFee, version = 3),
              marketSellOrder
            )
          case OrderType.BUY =>
            val buyOrder       = buy(wctWavesPair, amount = marketOrderAmount, price = 100, matcherFee = smallFee, version = 3, feeAsset = feeAsset)
            val marketBuyOrder = MarketOrder(buyOrder, availableForSpending = getSpentAmountWithFee(buyOrder))
            (
              sell(wctWavesPair, amount = counterOrderAmount, price = 90, matcherFee = smallFee, version = 3),
              marketBuyOrder
            )
        }

        withClue("Stop condition - no counter orders:") {
          orderBook ! wrapMarketOrder(marketOrder)
          val oc = tp.expectMsgType[OrderCanceled]

          oc.acceptedOrder shouldBe marketOrder
          oc.isSystemCancel shouldBe true
          obc.get(wctWavesPair).asks shouldBe empty

          tp.receiveN(0)
        }

        withClue("Stop condition - market order partially filled:") {
          orderBook ! wrapLimitOrder(counterOrder)
          tp.expectMsgType[OrderAdded]

          orderBook ! wrapMarketOrder(marketOrder)

          val oe = tp.expectMsgType[OrderExecuted]
          oe.submitted shouldBe marketOrder
          oe.counter shouldBe LimitOrder(counterOrder)
          oe.executedAmount shouldBe counterOrder.amount

          val oc2 = tp.expectMsgType[OrderCanceled]

          oc2.acceptedOrder shouldBe oe.submittedMarketRemaining(marketOrder)
          oc2.isSystemCancel shouldBe true

          eventually {
            obc.get(wctWavesPair).asks shouldBe empty
            obc.get(wctWavesPair).bids shouldBe empty
          }

          tp.receiveN(0)
        }
      }

      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.SELL, feeAsset = Waves)    // fee in received asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.SELL, feeAsset = ethAsset) // fee in third asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.SELL, feeAsset = wctAsset) // fee in spent asset

      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.BUY, feeAsset = wctAsset) // fee in received asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.BUY, feeAsset = ethAsset) // fee in third asset
      noCountersOrPartiallyFilledTest(marketOrderType = OrderType.BUY, feeAsset = Waves)    // fee in spent asset
    }

    "cancel market orders because of the stop conditions (available balance for spending is exhausted)" in {

      def afsIsNotEnoughTest(marketOrderType: OrderType, feeAsset: Asset, isFeeConsideredInExecutionAmount: Boolean): Unit =
        obcTest { (wctWavesPair, orderBook, tp) =>
          val moAmount             = toNormalized(4)
          val availableForSpending = moAmount / 2

          val (counterOrder, marketOrder) = marketOrderType match {
            case OrderType.SELL =>
              val buyOrder = buy(wctWavesPair, amount = moAmount, price = 100, matcherFee = smallFee, version = 3)
              val marketSellOrder =
                MarketOrder(
                  buyOrder.updateType(OrderType.SELL).updateMatcherFeeAssetId(feeAsset),
                  availableForSpending = availableForSpending
                )
              buyOrder -> marketSellOrder
            case OrderType.BUY =>
              val sellOrder = sell(wctWavesPair, amount = moAmount, price = 90, matcherFee = smallFee, version = 3)
              val marketBuyOrder =
                MarketOrder(
                  sellOrder.updateType(OrderType.BUY).updateMatcherFeeAssetId(feeAsset),
                  availableForSpending = availableForSpending
                )
              sellOrder -> marketBuyOrder
          }

          orderBook ! wrapLimitOrder(counterOrder)
          tp.expectMsgType[OrderAdded]

          orderBook ! wrapMarketOrder(marketOrder)

          val oe = tp.expectMsgType[OrderExecuted]
          oe.submitted shouldBe marketOrder
          oe.counter shouldBe LimitOrder(counterOrder)

          (marketOrderType, isFeeConsideredInExecutionAmount) match {
            case (OrderType.SELL, true)  => oe.executedAmount + oe.submittedExecutedFee shouldBe marketOrder.availableForSpending
            case (OrderType.SELL, false) => oe.executedAmount shouldBe marketOrder.availableForSpending

            case (OrderType.BUY, true) =>
              oe.executedAmountOfPriceAsset + oe.submittedExecutedFee should be <= marketOrder.availableForSpending
              MatcherModel.getCost(oe.executedAmount + 1, marketOrder.price) + oe.submittedExecutedFee should be > marketOrder.availableForSpending

            case (OrderType.BUY, false) =>
              oe.executedAmountOfPriceAsset should be <= marketOrder.availableForSpending
              MatcherModel.getCost(oe.executedAmount + 1, marketOrder.price) should be > marketOrder.availableForSpending
          }

          val oc = tp.expectMsgType[OrderCanceled]

          oc.acceptedOrder shouldBe oe.submittedMarketRemaining(marketOrder)
          oc.isSystemCancel shouldBe true

          tp.receiveN(0)

          eventually {
            obc.get(wctWavesPair).getSideFor(marketOrder) shouldBe empty
            obc.get(wctWavesPair).getCounterSideFor(marketOrder).map(_.amount).sum shouldBe counterOrder.amount - oe.executedAmount
          }
        }

      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = Waves, isFeeConsideredInExecutionAmount = false)    // fee in received asset
      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = ethAsset, isFeeConsideredInExecutionAmount = false) // fee in third asset
      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = wctAsset, isFeeConsideredInExecutionAmount = true)  // fee in spent asset

      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = wctAsset, isFeeConsideredInExecutionAmount = false) // fee in received asset
      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = ethAsset, isFeeConsideredInExecutionAmount = false) // fee in third asset
      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = Waves, isFeeConsideredInExecutionAmount = true)     // fee in spent asset
    }
  }
}
