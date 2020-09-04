package com.wavesplatform.dex.actors.orderbook

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.actor.typed.scaladsl.adapter._
import akka.testkit.{ImplicitSender, TestActorRef, TestProbe}
import cats.data.NonEmptyList
import cats.syntax.option._
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.actors.MatcherActor.SaveSnapshot
import com.wavesplatform.dex.actors.address.AddressActor.Command.Source
import com.wavesplatform.dex.actors.orderbook.OrderBookActor.{MarketStatus, OrderBookRecovered, OrderBookSnapshotUpdateCompleted}
import com.wavesplatform.dex.actors.{MatcherSpec, OrderBookAskAdapter}
import com.wavesplatform.dex.caches.OrderFeeSettingsCache
import com.wavesplatform.dex.db.OrderBookSnapshotDB
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.fixtures.RestartableActor
import com.wavesplatform.dex.fixtures.RestartableActor.RestartActor
import com.wavesplatform.dex.model.Events.{OrderAdded, OrderAddedReason, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.OrderBook.OrderBookUpdates
import com.wavesplatform.dex.model._
import com.wavesplatform.dex.queue.QueueEvent
import com.wavesplatform.dex.queue.QueueEvent.Canceled
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import com.wavesplatform.dex.settings.{DenormalizedMatchingRule, MatchingRule}
import com.wavesplatform.dex.time.SystemTime
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class OrderBookActorSpecification
    extends MatcherSpec("OrderBookActor")
    with SystemTime
    with ImplicitSender
    with MatcherSpecBase
    with PathMockFactory
    with Eventually {

  private val md = new ConcurrentHashMap[AssetPair, MarketStatus]

  private val wctAsset = IssuedAsset(ByteStr(Array.fill(32)(1)))
  private val ethAsset = IssuedAsset(ByteStr("ETH".getBytes))

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
                                 matchingRules: NonEmptyList[DenormalizedMatchingRule] = NonEmptyList.one(DenormalizedMatchingRule(0, 0.00000001)),
                                 makerTakerFeeAtOffset: Long => (AcceptedOrder, LimitOrder) => (Long, Long) = _ => makerTakerPartialFee)(
      f: (AssetPair, TestActorRef[OrderBookActor with RestartableActor], TestProbe) => Unit): Unit = {

    md.clear()

    val tp    = TestProbe()
    val pair  = AssetPair(wctAsset, Waves)
    val obsdb = OrderBookSnapshotDB.inMem

    prepare(obsdb, pair)

    implicit val efc: ErrorFormatterContext = ErrorFormatterContext.from(_ => 8)

    val orderBookActor = TestActorRef(
      new OrderBookActor(
        OrderBookActor.Settings(AggregatedOrderBookActor.Settings(100.millis)),
        tp.ref,
        tp.ref,
        system.actorOf(OrderBookSnapshotStoreActor.props(obsdb)),
        system.toTyped.ignoreRef,
        pair,
        time,
        matchingRules,
        _ => (),
        raw => MatchingRule(raw.startOffset, (raw.tickSize * BigDecimal(10).pow(8)).toLongExact),
        makerTakerFeeAtOffset,
        None
      ) with RestartableActor)

    f(pair, orderBookActor, tp)
    system.stop(orderBookActor)
  }

  "OrderBookActor" should {
    "recover from snapshot - 1" in obcTestWithPrepare((_, _) => ()) { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, None))
    }

    "recover from snapshot - 2" in obcTestWithPrepare { (obsdb, p) =>
      obsdb.update(p, 50L, Some(OrderBookSnapshot.empty))
    } { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "recovery - if there is a matching rule - DEX-775" in obcTestWithPrepare(
      prepare = (obsdb, p) => obsdb.update(p, 50L, Some(OrderBookSnapshot.empty)),
      matchingRules = NonEmptyList.of(DenormalizedMatchingRule(0, 0.00000001), DenormalizedMatchingRule(40, 0.0000001))
    ) { (pair, _, tp) =>
      tp.expectMsg(OrderBookRecovered(pair, Some(50)))
    }

    "recovery - notify address actor about orders" in obcTestWithPrepare(
      { (obsdb, p) =>
        val ord                                  = buy(p, 10 * Order.PriceConstant, 100)
        val ob                                   = OrderBook.empty
        val OrderBookUpdates(updatedOb, _, _, _) = ob.add(LimitOrder(ord), ord.timestamp, makerTakerPartialFee)
        obsdb.update(p, 50L, Some(updatedOb.snapshot))
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

      tp.receiveN(2) shouldEqual Seq(ord1, ord2).map(o => OrderAdded(LimitOrder(o), OrderAddedReason.OrderBookRecovered, o.timestamp))
      tp.expectMsgType[OrderBookRecovered]
    }

    "execute partial market orders and preserve remaining after restart" in obcTest { (pair, actor, tp) =>
      val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
      val ord2 = sell(pair, 15 * Order.PriceConstant, 100)

      actor ! wrapLimitOrder(ord1)
      tp.expectMsgType[OrderAdded]

      actor ! wrapLimitOrder(ord2)
      tp.expectMsgType[OrderAdded]
      tp.expectMsgType[OrderExecuted]

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]

      actor ! RestartActor
      tp.expectMsgType[OrderAdded] should matchTo(
        OrderAdded(SellLimitOrder(
            ord2.amount - ord1.amount,
            ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord1.amount),
            ord2,
            (BigInt(10) * Order.PriceConstant * 100 * Order.PriceConstant).bigInteger
          ), OrderAddedReason.OrderBookRecovered, ord2.timestamp)
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
      tp.receiveN(5)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount - ord3.amount
      tp.expectMsg(
        OrderAdded(BuyLimitOrder(
            restAmount,
            ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2,
            (BigInt(2) * Order.PriceConstant * 100 * Order.PriceConstant).bigInteger
          ), OrderAddedReason.OrderBookRecovered, ord2.timestamp)
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
      tp.receiveN(7)

      actor ! SaveSnapshot(Long.MaxValue)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      actor ! RestartActor

      val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount
      tp.expectMsg(
        OrderAdded(SellLimitOrder(
            restAmount,
            ord2.matcherFee - AcceptedOrder.partialFee(ord2.matcherFee, ord2.amount, ord2.amount - restAmount),
            ord2,
            (BigInt(4) * Order.PriceConstant * 100 * Order.PriceConstant).bigInteger
          ), OrderAddedReason.OrderBookRecovered, ord2.timestamp)
      )
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
        actor ! wrapLimitOrder(i, buy(pair, 100000000L, 0.00041))
      }
      tp.receiveN(10)

      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000L, 0.00041))
      }
      tp.expectNoMessage(100.millis)
    }

    "respond on SaveSnapshotCommand" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000L, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10L)
      tp.expectMsg(OrderBookSnapshotUpdateCompleted(pair, Some(10)))

      (11 to 20).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000L, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(20L)
      tp.expectMsg(OrderBookSnapshotUpdateCompleted(pair, Some(20)))
    }

    "don't do a snapshot if there is no changes" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000L, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10L)
      actor ! SaveSnapshot(10L)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
      tp.expectNoMessage(200.millis)
    }

    "restore its state at start" in obcTest { (pair, actor, tp) =>
      (1 to 10).foreach { i =>
        actor ! wrapLimitOrder(i, buy(pair, 100000000L, 0.00041))
      }
      tp.receiveN(10)

      actor ! SaveSnapshot(10L)
      tp.expectMsgType[OrderBookSnapshotUpdateCompleted]
    }

    "cancel order with price not equal to it's level price" in obcTestWithTickSize(100) { (pair, orderBook, tp) =>
      val buyOrder = buy(pair, 100000000L, 0.0000041)

      orderBook ! wrapLimitOrder(1, buyOrder)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapEvent(2, Canceled(buyOrder.assetPair, buyOrder.id(), Source.Request))
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
      val buyOrder = buy(pair, 100000000L, 0.0000041)
      (0 to 17).foreach { i =>
        orderBook ! wrapLimitOrder(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = getAggregatedSnapshot(orderBook).bids
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
      val buyOrder = buy(pair, 100000000L, 0.0000041)
      (0 to 10).foreach { i =>
        orderBook ! wrapLimitOrder(i, buyOrder)
        tp.expectMsgType[OrderAdded]
      }

      eventually {
        val bids = getAggregatedSnapshot(orderBook).bids
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
      val buyOrder1, buyOrder2 = buy(pair, 100000000L, 0.0000041)

      orderBook ! wrapLimitOrder(0, buyOrder1) // order book places order to the price level 41
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapLimitOrder(1, buyOrder2) // now order book places the same order to the price level 40
      tp.expectMsgType[OrderAdded]

      eventually {
        val bids = getAggregatedSnapshot(orderBook).bids
        bids.size shouldBe 2
        bids.head.price shouldBe buyOrder1.price
        bids.last.price shouldBe 0.0000040 * Order.PriceConstant
      }

      orderBook ! wrapEvent(2, Canceled(buyOrder1.assetPair, buyOrder1.id(), Source.Request)) // order book is looking for the price level of buyOrder1 correctly (41 but not 40)
      tp.expectMsgType[OrderCanceled]

      eventually {
        val bids = getAggregatedSnapshot(orderBook).bids
        bids.size shouldBe 1
        bids.head.price shouldBe 0.000004 * Order.PriceConstant
      }
    }

    "correctly apply new fees when rules are switched" in {
      val makerTakerFeeAtOffset = Fee.getMakerTakerFeeByOffset(
        new OrderFeeSettingsCache(
          Map(
            0L -> DynamicSettings(0.001.waves, 0.003.waves),
            1L -> DynamicSettings(0.001.waves, 0.005.waves)
          )
        )
      ) _

      obcTestWithPrepare(prepare = (_, _) => (), makerTakerFeeAtOffset = makerTakerFeeAtOffset) { (pair, orderBook, tp) =>
        tp.expectMsg(OrderBookRecovered(pair, None))
        val now = System.currentTimeMillis()

        // place when order fee settings is DynamicSettings(0.001.waves, 0.003.waves)
        val maker1 = sell(wavesUsdPair, 10.waves, 3.00, matcherFee = 0.003.waves.some, ts = now.some)
        orderBook ! wrapLimitOrder(0, maker1)
        tp.expectMsgType[OrderAdded]

        // place when order fee settings is DynamicSettings(0.001.waves, 0.005.waves)
        val taker1 = buy(wavesUsdPair, 10.waves, 3.00, matcherFee = 0.005.waves.some, ts = now.some)
        orderBook ! wrapLimitOrder(1, taker1)
        tp.expectMsgType[OrderAdded]

        val oe = tp.expectMsgType[OrderExecuted]
        oe.counterExecutedFee shouldBe 0.0006.waves
        oe.submittedExecutedFee shouldBe 0.005.waves
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
        tp.expectMsgType[OrderAdded]
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
          val ob = getAggregatedSnapshot(orderBook)
          ob.getCounterSideFor(marketOrder).map(_.amount).sum shouldBe toNormalized(2)
          ob.getSideFor(marketOrder) shouldBe empty
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
          tp.expectMsgType[OrderAdded]
          val oc = tp.expectMsgType[OrderCanceled]

          oc.acceptedOrder shouldBe marketOrder
          oc.reason shouldBe Events.OrderCanceledReason.BecameUnmatchable
          getAggregatedSnapshot(orderBook).asks shouldBe empty

          tp.receiveN(0)
        }

        withClue("Stop condition - market order partially filled:") {
          orderBook ! wrapLimitOrder(counterOrder)
          tp.expectMsgType[OrderAdded]

          orderBook ! wrapMarketOrder(marketOrder)
          tp.expectMsgType[OrderAdded]
          val oe = tp.expectMsgType[OrderExecuted]
          oe.submitted shouldBe marketOrder
          oe.counter shouldBe LimitOrder(counterOrder)
          oe.executedAmount shouldBe counterOrder.amount

          val oc2 = tp.expectMsgType[OrderCanceled]

          oc2.acceptedOrder shouldBe oe.submittedMarketRemaining(marketOrder)
          oc2.reason shouldBe Events.OrderCanceledReason.BecameUnmatchable

          eventually {
            val ob = getAggregatedSnapshot(orderBook)
            ob.asks shouldBe empty
            ob.bids shouldBe empty
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
                  buyOrder.updateType(OrderType.SELL).updateFeeAsset(feeAsset),
                  availableForSpending = availableForSpending
                )
              buyOrder -> marketSellOrder
            case OrderType.BUY =>
              val sellOrder = sell(wctWavesPair, amount = moAmount, price = 90, matcherFee = smallFee, version = 3)
              val marketBuyOrder =
                MarketOrder(
                  sellOrder.updateType(OrderType.BUY).updateFeeAsset(feeAsset),
                  availableForSpending = availableForSpending
                )
              sellOrder -> marketBuyOrder
          }

          orderBook ! wrapLimitOrder(counterOrder)
          tp.expectMsgType[OrderAdded]

          orderBook ! wrapMarketOrder(marketOrder)

          tp.expectMsgType[OrderAdded]
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
          oc.reason shouldBe Events.OrderCanceledReason.BecameUnmatchable

          tp.receiveN(0)

          eventually {
            val ob = getAggregatedSnapshot(orderBook)
            ob.getSideFor(marketOrder) shouldBe empty
            ob.getCounterSideFor(marketOrder).map(_.amount).sum shouldBe counterOrder.amount - oe.executedAmount
          }
        }

      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = Waves, isFeeConsideredInExecutionAmount = false)    // fee in received asset
      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = ethAsset, isFeeConsideredInExecutionAmount = false) // fee in third asset
      afsIsNotEnoughTest(marketOrderType = OrderType.SELL, feeAsset = wctAsset, isFeeConsideredInExecutionAmount = true)  // fee in spent asset

      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = wctAsset, isFeeConsideredInExecutionAmount = false) // fee in received asset
      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = ethAsset, isFeeConsideredInExecutionAmount = false) // fee in third asset
      afsIsNotEnoughTest(marketOrderType = OrderType.BUY, feeAsset = Waves, isFeeConsideredInExecutionAmount = true)     // fee in spent asset
    }

    "cancel a submitted order if it couldn't be matched by a price of a counter order" in obcTest { (wctWavesPair, orderBook, tp) =>
      val counterOrder = rawSell(
        pair = wctWavesPair,
        amount = 20000L,
        price = 90000L,
        matcherFee = Some(300000L),
        version = 1.toByte
      )

      val submittedOrder = rawBuy(
        pair = wctWavesPair,
        amount = 1000L,
        price = 100000L,
        matcherFee = Some(300000L),
        version = 3.toByte
      )

      orderBook ! wrapLimitOrder(counterOrder)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapLimitOrder(submittedOrder)
      tp.expectMsgType[OrderAdded]
      // The amount=1000 should >= ceil(10^8 / 90000) = 1112
      tp.expectMsgType[OrderCanceled].reason shouldBe Events.OrderCanceledReason.BecameUnmatchable
    }

    "orders are cancelling after during an order book deletion" in obcTest { (wctWavesPair, orderBook, tp) =>
      val order = rawSell(
        pair = wctWavesPair,
        amount = 20000L,
        price = 90000L,
        matcherFee = Some(300000L),
        version = 1.toByte
      )

      orderBook ! wrapLimitOrder(order)
      tp.expectMsgType[OrderAdded]

      orderBook ! wrapEvent(QueueEvent.OrderBookDeleted(wctWavesPair))
      tp.expectMsgType[OrderCanceled].reason shouldBe Events.OrderCanceledReason.OrderBookDeleted
    }
  }

  private def getAggregatedSnapshot(orderBookRef: ActorRef): OrderBookAggregatedSnapshot = {
    val pair       = wavesUsdPair // hack
    val askAdapter = new OrderBookAskAdapter(new AtomicReference(Map(pair -> Right(orderBookRef))), 5.seconds)
    Await.result(askAdapter.getAggregatedSnapshot(pair), 1.second).toOption.flatten.getOrElse(throw new IllegalStateException("Can't get snapshot"))
  }
}
