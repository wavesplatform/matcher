package com.wavesplatform.dex.model

import java.math.BigInteger
import java.nio.ByteBuffer

import cats.syntax.semigroup._
import com.wavesplatform.dex.codecs.OrderBookSideSnapshotCodecs
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.{Normalization, Price}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderAddedReason, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.OrderBook.OrderBookUpdates
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import com.wavesplatform.dex.settings.{MatchingRule, OrderFeeSettings}
import com.wavesplatform.dex.time.SystemTime
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.immutable.{Queue, TreeMap}
import scala.collection.mutable

class OrderBookTestSuite
    extends AnyFreeSpec
    with PropertyChecks
    with Matchers
    with MatcherSpecBase
    with NoShrink
    with TableDrivenPropertyChecks
    with SystemTime {

  def getDefaultMakerTakerFee(s: AcceptedOrder, c: LimitOrder): (Long, Long) = Fee.getMakerTakerFee(DynamicSettings.symmetric(300000L))(s, c)

  implicit class OrderBookOps(ob: OrderBook) {
    def append(ao: AcceptedOrder, ts: Long, tickSize: Long = MatchingRule.DefaultRule.tickSize): OrderBookUpdates =
      ob.add(ao, ts, (t, m) => getDefaultMakerTakerFee(t, m), tickSize)

    def appendLimit(o: Order, ts: Long, tickSize: Long = MatchingRule.DefaultRule.tickSize): OrderBookUpdates =
      append(LimitOrder(o), ts, tickSize)

    def appendAll[T](xs: Seq[T])(f: (OrderBook, T) => OrderBookUpdates): OrderBookUpdates =
      xs.foldLeft(OrderBookUpdates(ob, Queue.empty[Event], LevelAmounts.empty, None)) {
        case (currentUpdates, x) =>
          val newUpdates = f(currentUpdates.orderBook, x)
          newUpdates.copy(events = currentUpdates.events ++ newUpdates.events, levelChanges = currentUpdates.levelChanges |+| newUpdates.levelChanges)
      }

    def appendAllAccepted(xs: Seq[AcceptedOrder], ts: Long, tickSize: Long = MatchingRule.DefaultRule.tickSize): OrderBookUpdates =
      appendAll(xs)(_.append(_, ts, tickSize))

    def appendAllLimit(xs: Seq[Order], ts: Long, tickSize: Long = MatchingRule.DefaultRule.tickSize): OrderBookUpdates =
      appendAllAccepted(xs.map(LimitOrder(_)), ts, tickSize)
  }

  private val now     = time.getTimestamp()
  val pair: AssetPair = AssetPair(Waves, mkAssetId("BTC"))

  "place buy orders with different prices" in {
    val ord1 = LimitOrder(buy(pair, 1583290045643L, 34118))
    val ord2 = LimitOrder(buy(pair, 170484969L, 34120))
    val ord3 = LimitOrder(buy(pair, 44521418496L, 34000))

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllAccepted(List(ord1, ord2, ord3), now)
    ob.allOrders.toList should matchTo(List(ord2, ord1, ord3))
  }

  "place several buy orders at the same price" in {}

  "place orders with different tick sizes" in {
    val normalizedTickSizes =
      Array(1L, 7L, 8L, 100L, 211L, 320L, 100000L, 124337L, 250250L, toNormalized(10L), 651983183L, toNormalized(100L))
    val prices = Gen.choose(1, 100000L)

    val gen = Gen
      .zip(Gen.oneOf(normalizedTickSizes.toSeq), Gen.listOf(prices), Gen.listOf(prices))
      .map {
        case (tickSize, askPrices, bidPrices) =>
          val asks = askPrices.map(x => sell(pair, 984651354686L, x))
          val bids = bidPrices.map(x => buy(pair, 1583290045643L, x))
          (tickSize, OrderBook.empty.appendAllLimit(asks ++ bids, now, tickSize = tickSize).orderBook)
      }

    forAll(gen) {
      case (tickSize, ob) =>
        for ((level, orders) <- ob.bids.iterator; order <- orders) {
          order.price - level should be < tickSize
          level % tickSize shouldBe 0
        }
        for ((level, orders) <- ob.asks.iterator; order <- orders) {
          level - order.price should be < tickSize
          level % tickSize shouldBe 0
        }
    }
  }

  "place buy and sell orders with prices different from each other less than tick size in one level" in {
    val tickSize           = 100L
    val normalizedTickSize = toNormalized(tickSize)

    val buyOrd1    = LimitOrder(buy(pair, 1583290045643L, 34100))
    val buyOrd2    = LimitOrder(buy(pair, 170484969L, 34120))
    val buyOrd3    = LimitOrder(buy(pair, 44521418496L, 34210))
    val buyOrd4    = LimitOrder(buy(pair, 44521418495L, 34303))
    val buyOrd5    = LimitOrder(buy(pair, 44521418494L, 34357))
    val buyOrd6    = LimitOrder(buy(pair, 44521418493L, 34389))
    val buyOrdZero = LimitOrder(buy(pair, 44521418493L, 90))

    val sellOrd1 = LimitOrder(sell(pair, 2583290045643L, 44100))
    val sellOrd2 = LimitOrder(sell(pair, 270484969L, 44120))
    val sellOrd3 = LimitOrder(sell(pair, 54521418496L, 44210))
    val sellOrd4 = LimitOrder(sell(pair, 54521418495L, 44303))
    val sellOrd5 = LimitOrder(sell(pair, 54521418494L, 44357))
    val sellOrd6 = LimitOrder(sell(pair, 54521418493L, 44389))

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllAccepted(
      List(buyOrd1, buyOrd2, buyOrd3, buyOrd4, buyOrd5, buyOrd6, buyOrdZero, sellOrd1, sellOrd2, sellOrd3, sellOrd4, sellOrd5, sellOrd6),
      now,
      normalizedTickSize
    )

    ob.asks.keySet should matchTo(Set(44100L, 44200L, 44300L, 44400L).map(toNormalized))
    ob.bids.keySet should matchTo(Set(0L, 34300L, 34200L, 34100L).map(toNormalized))

    ob.bids shouldBe TreeMap[Price, Level](
      Seq(
        0     -> Queue(buyOrdZero),
        34300 -> Queue(buyOrd4, buyOrd5, buyOrd6),
        34200 -> Queue(buyOrd3),
        34100 -> Queue(buyOrd1, buyOrd2)
      ).map { case (price, orders) => toNormalized(price) -> orders }: _*
    )

    ob.asks shouldBe TreeMap[Price, Level](
      Seq(
        44100 -> Queue(sellOrd1),
        44200 -> Queue(sellOrd2),
        44300 -> Queue(sellOrd3),
        44400 -> Queue(sellOrd4, sellOrd5, sellOrd6)
      ).map { case (price, orders) => toNormalized(price) -> orders }: _*
    )

    val sellTickSizeOrder = sell(pair, 54521418493L, 40)
    OrderBook.empty.appendLimit(sellTickSizeOrder, ntpNow, tickSize = normalizedTickSize).orderBook.asks should matchTo(
      TreeMap[Price, Level](
        Seq(
          tickSize -> Queue(sellTickSizeOrder)
        ).map { case (price, orders) => toNormalized(price) -> orders.map(LimitOrder.apply) }: _*
      ))
  }

  "place matchable orders with and without tick size" in {
    val amt                = 54521418493L
    val normalizedTickSize = toNormalized(8)

    val counterSellOrder  = LimitOrder(sell(pair, amt, 9))
    val submittedBuyOrder = LimitOrder(buy(pair, amt, 10))

    val ob1 = withClue("matchable orders should be matched without tick size:\n") {

      val OrderBookUpdates(ob1, events1, _, _) = OrderBook.empty.append(counterSellOrder, now)
      val OrderBookUpdates(ob, events2, _, _)  = ob1.append(submittedBuyOrder, now + 1)
      val events                               = events1 ++ events2

      events should matchTo(
        Queue[Event](
          OrderAdded(counterSellOrder, OrderAddedReason.RequestExecuted, now),
          OrderAdded(submittedBuyOrder, OrderAddedReason.RequestExecuted, now + 1),
          OrderExecuted(submittedBuyOrder, counterSellOrder, now + 1, counterSellOrder.matcherFee, submittedBuyOrder.matcherFee)
        )
      )

      ob.asks shouldBe empty
      ob.bids shouldBe empty
      ob
    }

    withClue("matchable orders should not be matched with tick size:\n") {
      val OrderBookUpdates(ob, events, _, _) = ob1.appendAllAccepted(List(counterSellOrder, submittedBuyOrder), now, tickSize = normalizedTickSize)

      withClue(s"Events: $events\n") {
        ob.asks should matchTo(
          TreeMap[Price, Level](
            Seq(16 -> Queue(counterSellOrder)).map { case (price, orders) => toNormalized(price) -> orders }: _*
          )
        )

        ob.bids should matchTo(
          TreeMap[Price, Level](
            Seq(8 -> Queue(submittedBuyOrder)).map { case (price, orders) => toNormalized(price) -> orders }: _*
          )
        )
      }
    }
  }

  "old counter orders should be matched with the new ones (with new activated tick-size)" in {
    def normalizedTickSize(tickSize: Double): Price = Normalization.normalizePrice(tickSize, 8, 2)

    val counter   = LimitOrder(createOrder(wavesUsdPair, SELL, 100.waves, 3.15))
    val submitted = LimitOrder(createOrder(wavesUsdPair, BUY, 100.waves, 3.15))

    val counterTs   = counter.order.timestamp
    val submittedTs = submitted.order.timestamp

    withClue("Counter SELL order (price = 3.15, tick size disabled) and submitted BUY order (price = 3.15, tick size = 0.1) should be matched:\n") {
      val OrderBookUpdates(ob1, events1, _, _) = OrderBook.empty.append(counter, counterTs)
      events1 should matchTo(Queue[Event](OrderAdded(counter, OrderAddedReason.RequestExecuted, counterTs)))

      val OrderBookUpdates(ob2, events2, _, _) = ob1.append(submitted, submittedTs, tickSize = normalizedTickSize(0.1))
      events2 should matchTo(
        Queue[Event](
          OrderAdded(submitted, OrderAddedReason.RequestExecuted, submittedTs),
          OrderExecuted(submitted, counter, submittedTs, submitted.matcherFee, counter.matcherFee)
        ))

      ob2.asks shouldBe empty
      ob2.bids shouldBe empty
    }
  }

  "place several sell orders at the same price" - {
    "with same level" - {
      "TickSize.Enabled" in {
        val normalizedTickSize = toNormalized(100L)

        val sellOrder                     = LimitOrder(sell(pair, 54521418493L, 44389))
        val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllAccepted(List(sellOrder, sellOrder), now, tickSize = normalizedTickSize)

        ob.asks should have size 1
        ob.asks.head._2.toList should matchTo(List(sellOrder, sellOrder))
      }

      "MatchingRules.Default.normalizedTickSize" in {
        val sellOrder = LimitOrder(sell(pair, 54521418493L, 44389))

        val OrderBookUpdates(ob, _, _, _) =
          OrderBook.empty.appendAllAccepted(List(sellOrder, sellOrder), now, tickSize = MatchingRule.DefaultRule.tickSize)

        ob.asks should have size 1
        ob.asks.head._2.toList should matchTo(List(sellOrder, sellOrder))
      }
    }

    "with different levels" in {
      val sellOrder = LimitOrder(sell(pair, 54521418493L, 44389))

      val OrderBookUpdates(ob, _, _, _) =
        OrderBook.empty
          .append(sellOrder, ntpNow, tickSize = toNormalized(100L))
          .orderBook
          .append(sellOrder, ntpNow, tickSize = MatchingRule.DefaultRule.tickSize)

      ob.asks should have size 2
      ob.asks.flatMap(_._2).toList should matchTo(List(sellOrder, sellOrder))
    }
  }

  "sell market" in {
    val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
    val ord2 = buy(pair, 10 * Order.PriceConstant, 105)

    val OrderBookUpdates(ob1, _, _, _) = OrderBook.empty.appendAllLimit(List(ord1, ord2), now)
    ob1.allOrders.toList should matchTo {
      List[LimitOrder](BuyLimitOrder(ord2.amount, ord2.matcherFee, ord2, BigInteger.ZERO),
                       BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1, BigInteger.ZERO))
    }

    val ord3                           = sell(pair, 10 * Order.PriceConstant, 100)
    val OrderBookUpdates(ob2, _, _, _) = ob1.append(LimitOrder(ord3), now)

    ob2.allOrders.toList should matchTo(List[LimitOrder](BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1, BigInteger.ZERO)))
  }

  "execute orders at different price levels" in {

    val ord1 = sell(pair, 10 * Order.PriceConstant, 100, matcherFee = Some(300000))
    val ord2 = sell(pair, 5 * Order.PriceConstant, 110, matcherFee = Some(300000))
    val ord3 = sell(pair, 10 * Order.PriceConstant, 110, matcherFee = Some(300000))
    val ord4 = buy(pair, 22 * Order.PriceConstant, 115, matcherFee = Some(300000))

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllAccepted(List(ord1, ord2, ord3, ord4).map(LimitOrder.apply), now)
    val restAmount                    = ord1.amount + ord2.amount + ord3.amount - ord4.amount

    ob.allOrders.toList should matchTo(
      List[LimitOrder](
        SellLimitOrder(
          restAmount,
          ord3.matcherFee - AcceptedOrder.partialFee(ord3.matcherFee, ord3.amount, ord3.amount - restAmount),
          ord3,
          (BigInt(7) * Order.PriceConstant * 110 * Order.PriceConstant).bigInteger
        )
      )
    )
  }

  "partially execute order with small remaining part" in {
    val ord1 = sell(pair, 200000000L, 0.00041)
    val ord2 = sell(pair, 100000000L, 0.0004)
    val ord3 = buy(pair, 100000001L, 0.00045)

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllAccepted(List(ord1, ord2, ord3).map(LimitOrder(_)), now)
    ob.allOrders.toList should matchTo(List[LimitOrder](SellLimitOrder(ord1.amount, ord1.matcherFee, ord1, BigInteger.ZERO)))
  }

  "partially execute order with zero fee remaining part" in {
    val ord1 = sell(pair, 1500.waves, 0.00069990)
    val ord2 = sell(pair, 30.75248828.waves, 0.00067634)
    val ord3 = buy(pair, 30.75363900.waves, 0.00073697)

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllAccepted(List(ord1, ord2, ord3).map(LimitOrder(_)), now)

    val corrected1 = Order.correctAmount(ord2.amount, ord2.price)
    val leftovers1 = ord3.amount - corrected1
    val corrected2 = Order.correctAmount(leftovers1, ord1.price)
    val restAmount = ord1.amount - corrected2
    // See OrderExecuted.submittedRemainingFee
    val restFee = ord1.matcherFee - AcceptedOrder.partialFee(ord1.matcherFee, ord1.amount, corrected2)
    ob.allOrders.toList should matchTo(List[LimitOrder](SellLimitOrder(restAmount, restFee, ord1, (BigInt(corrected2) * 69990L).bigInteger)))
  }

  "partially execute order with price > 1 and zero fee remaining part " in {
    val pair = AssetPair(IssuedAsset(ByteStr("BTC".getBytes)), IssuedAsset(ByteStr("USD".getBytes)))
    val ord1 = sell(pair, 0.1.waves, 1850)
    val ord2 = sell(pair, 0.01.waves, 1840)
    val ord3 = buy(pair, 0.01000010.waves, 2000)

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllLimit(List(ord1, ord2, ord3), now)

    val restAmount = ord1.amount - (ord3.amount - ord2.amount)
    val restFee    = ord1.matcherFee - AcceptedOrder.partialFee(ord1.matcherFee, ord1.amount, ord3.amount - ord2.amount)
    ob.allOrders.toList should matchTo {
      List[LimitOrder](SellLimitOrder(restAmount, restFee, ord1, (BigInt(1850) * Order.PriceConstant * 0.00000010.waves).bigInteger))
    }
  }

  "buy small amount of pricey asset" in {
    val p = AssetPair(IssuedAsset(ByteStr("WAVES".getBytes)), IssuedAsset(ByteStr("USD".getBytes)))
    val b = rawBuy(p, 0.007.waves, 280L)
    val s = rawSell(p, 300.0.waves, 280L)

    val OrderBookUpdates(ob, _, _, _) = OrderBook.empty.appendAllLimit(List(s, b), now)

    val restSAmount = Order.correctAmount(700000L, 280)
    val restAmount  = 30000000000L - restSAmount
    val restFee     = s.matcherFee - AcceptedOrder.partialFee(s.matcherFee, s.amount, restSAmount)
    ob.allOrders.toList should matchTo(List[LimitOrder](SellLimitOrder(restAmount, restFee, s, (BigInt(restSAmount) * 280).bigInteger)))
  }

  "cleanup expired buy orders" in {
    pending
  }

  "cleanup expired sell orders" in {
    pending
  }

  "aggregate levels for snapshot, preserving order" in {
    pending
  }

  "LimitOrder serialization" in forAll(limitOrderGenerator) { x =>
    import com.wavesplatform.dex.codecs.OrderBookSideSnapshotCodecs.{encodeLoV1, encodeLoV2}
    Seq(encodeLoV1 _, encodeLoV2 _).foreach { encode =>
      val dest = new mutable.ArrayBuilder.ofByte
      encode(dest, x)
      val bb = ByteBuffer.wrap(dest.result())
      OrderBookSideSnapshotCodecs.decodeLo(bb) should matchTo(x)
    }
  }

  "OrderBookSideSnapshot serialization" in forAll(sideSnapshotSerGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    OrderBookSideSnapshotCodecs.encode(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    OrderBookSideSnapshotCodecs.decode(bb) should matchTo(x)
  }

  "LastTrade serialization" in forAll(lastTradeGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    LastTrade.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    LastTrade.fromBytes(bb) should matchTo(x)
  }

  "OrderBookSnapshot serialization" in forAll(snapshotGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    OrderBookSnapshot.serialize(dest, x)
    val bb       = ByteBuffer.wrap(dest.result())
    val restored = OrderBookSnapshot.fromBytes(bb)
    restored.asks should matchTo(x.asks)
    restored.bids should matchTo(x.bids)
    restored.lastTrade should matchTo(x.lastTrade)
  }

  "create OrderExecuted events with different executed fee for maker and taker" in {

    def limit(amount: Long, orderType: OrderType, fee: Long, feeAsset: Asset = Waves): LimitOrder =
      LimitOrder(createOrder(wavesUsdPair, orderType, amount, 3.00, fee, feeAsset = feeAsset))

    def market(amount: Long, orderType: OrderType, fee: Long, feeAsset: Asset): MarketOrder =
      MarketOrder(createOrder(wavesUsdPair, orderType, amount, 3.00, fee, feeAsset = feeAsset), _ => Long.MaxValue)

    forAll(
      Table(
        ("M amount", "T amount", "is T market", "fee settings", "M order fee", "T order fee", "T fee asset", "M executed fee", "T executed fee"),
        /** symmetric */
        (1.waves, 1.waves, false, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.003, Waves, 0.003.waves, 0.003.waves), // like in old good times
        (1.waves, 1.waves, false, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.009, Waves, 0.001.waves, 0.009.waves), // orders have sufficient fee = 0.009.waves
        /** small maker - big taker */
        (2.waves, 10.waves, false, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.003, Waves, 0.003.waves, 0.0006.waves), // like in old good times
        (2.waves, 10.waves, false, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.009, Waves, 0.001.waves, 0.0018.waves), // orders have sufficient fee = 0.009.waves
        /** big maker - small taker */
        (10.waves, 2.waves, false, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.003, Waves, 0.0006.waves, 0.003.waves), // like in old good times
        (10.waves, 2.waves, false, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.009, Waves, 0.0002.waves, 0.009.waves), // orders have sufficient fee = 0.009.waves
        /** symmetric, taker is market */
        (1.waves, 1.waves, true, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.003, Waves, 0.003.waves, 0.003.waves), // like in old good times
        (1.waves, 1.waves, true, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.009, Waves, 0.001.waves, 0.009.waves), // orders have sufficient fee = 0.009.waves
        /** small maker - big market taker */
        (2.waves, 10.waves, true, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.003, Waves, 0.003.waves, 0.0006.waves), // like in old good times
        (2.waves, 10.waves, true, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.009, Waves, 0.001.waves, 0.0018.waves), // orders have sufficient fee = 0.009.waves
        /** big maker - small market taker */
        (10.waves, 2.waves, true, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.003, Waves, 0.0006.waves, 0.003.waves), // like in old good times
        (10.waves, 2.waves, true, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.009, Waves, 0.0002.waves, 0.009.waves), // orders have sufficient fee = 0.009.waves
        /** symmetric, taker fee in ETH */
        (1.waves, 1.waves, false, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.00001703, eth, 0.003.waves, 0.00001703.eth), // like in old good times
        (1.waves, 1.waves, false, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.00005109, eth, 0.001.waves, 0.00005109.eth), // orders have sufficient fee = 0.009.waves = 0.00005109.eth
        /** small maker - big taker, taker fee in ETH */
        (2.waves, 10.waves, false, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.00001703, eth, 0.003.waves, 0.00000340.eth), // like in old good times
        (2.waves, 10.waves, false, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.00005109, eth, 0.001.waves, 0.00001021.eth), // orders have sufficient fee = 0.009.waves = 0.00005109.eth
        /** big maker - small taker, taker fee in ETH */
        (10.waves, 2.waves, false, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.00001703, eth, 0.0006.waves, 0.00001703.eth), // like in old good times
        (10.waves, 2.waves, false, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.00005109, eth, 0.0002.waves, 0.00005109.eth), // orders have sufficient fee = 0.009.waves = 0.00005109.eth
        /** symmetric, taker is market, taker fee in ETH */
        (1.waves, 1.waves, true, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.00001703, eth, 0.003.waves, 0.00001703.eth), // like in old good times
        (1.waves, 1.waves, true, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.00005109, eth, 0.001.waves, 0.00005109.eth), // orders have sufficient fee = 0.009.waves = 0.00005109.eth
        /** small maker - big market taker, taker fee in ETH */
        (2.waves, 10.waves, true, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.00001703, eth, 0.003.waves, 0.00000340.eth), // like in old good times
        (2.waves, 10.waves, true, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.00005109, eth, 0.001.waves, 0.00001021.eth), // orders have sufficient fee = 0.009.waves = 0.00005109.eth
        /** big maker - small market taker, taker fee in ETH */
        (10.waves, 2.waves, true, DynamicSettings(0.003.waves, 0.003.waves), 0.003.waves, 0.00001703, eth, 0.0006.waves, 0.00001703.eth), // like in old good times
        (10.waves, 2.waves, true, DynamicSettings(0.001.waves, 0.009.waves), 0.009.waves, 0.00005109, eth, 0.0002.waves, 0.00005109.eth) // orders have sufficient fee = 0.009.waves = 0.00005109.eth
      )
      // format: off
    ) { (mAmt: Long, tAmt: Long, isTMarket: Boolean, ofs: OrderFeeSettings, orderMFee: Long, orderTFee: Double, tFeeAsset: Asset, eMFee: Long, eTFee: Long) =>
      // format: on
      val normalizedOrderTFee = Normalization.normalizeAmountAndFee(orderTFee, 8)

      val maker = limit(mAmt, SELL, orderMFee)
      val taker = if (isTMarket) market(tAmt, BUY, normalizedOrderTFee, tFeeAsset) else limit(tAmt, BUY, normalizedOrderTFee, tFeeAsset)
      val gmtf  = Fee.getMakerTakerFee(ofs)(_, _)

      // Ignore first two OrderAdded, take next OrderExecuted, other events aren't interesting
      val evt = OrderBook.empty.appendAll(List(maker, taker))(_.add(_, now, gmtf)).events.drop(2).head

      evt shouldBe a[OrderExecuted]
      val oe = evt.asInstanceOf[OrderExecuted]
      oe.counterExecutedFee shouldBe eMFee
      oe.submittedExecutedFee shouldBe eTFee
    }
  }

  "not create OrderExecuted event with executed amount = 0 and the last trade should not have amount = 0" in {
    val sellOrder = LimitOrder(rawSell(pair, 345506L, 9337000000L))
    val buyOrder  = MarketOrder(rawBuy(pair, 80902L, 10270700000L), 26L)

    val r = OrderBook.empty.appendAllAccepted(List(sellOrder, buyOrder), now)
    r.events should have size (3)
    val lastEvent = r.events.last match {
      case x: OrderCanceled => x
      case x                => fail(s"Expected cancel, but got $x")
    }

    lastEvent should matchTo(OrderCanceled(buyOrder, Events.OrderCanceledReason.BecameUnmatchable, 0L))
    r.lastTrade.isEmpty shouldBe true
  }

  "correctly calculates average price" in {

    val balance   = Map[Asset, Long](usd -> 500.usd).withDefaultValue(0L)
    val submitted = createOrder(wavesUsdPair, BUY, 200.waves, 3.0)

    Seq(LimitOrder(submitted), MarketOrder(submitted, a => balance(a))).foreach { buy =>
      val orderType = if (buy.isLimit) "Limit order" else "Market order"
      val ob        = OrderBook.empty

      val sell1 = LimitOrder(createOrder(wavesUsdPair, SELL, 10.waves, 2.5))
      val sell2 = LimitOrder(createOrder(wavesUsdPair, SELL, 50.waves, 2.7))
      val sell3 = LimitOrder(createOrder(wavesUsdPair, SELL, 110.waves, 3.0))

      val OrderBookUpdates(_, events, _, _) = ob.appendAll(Seq(sell1, sell2, sell3, buy).zipWithIndex) {
        case (ob, (order, timeOffset)) => ob.add(order, now + timeOffset, getDefaultMakerTakerFee)
      }

      events should have size (if (buy.isLimit) 7 else 8)
      (0 to 3).foreach(events(_) shouldBe a[OrderAdded])

      forAll(
        Table(
          ("event id", "execution number", "submitted weighed price", "submitted avg price", "counter weighed price", "counter avg price"),
          (4, "first", 25.waves.usd, 2.5.usd, 25.waves.usd, 2.5.usd),
          (5, "second", 160.waves.usd, 2.66.usd, 135.waves.usd, 2.7.usd),
          (6, "third", 490.waves.usd, 2.88.usd, 330.waves.usd, 3.0.usd),
        )
        // format: off
      ) { (eventId: Int, executionNumber: String, sAvgWeighedPriceNominator: Long, submittedAvgPrice: Long, cAvgWeighedPriceNominator: Long, counterAvgPrice: Long) =>
        // format: on

        withClue(s"$orderType, $executionNumber execution of 3:\n") {
          events(eventId) shouldBe a[OrderExecuted]
          val oe = events(eventId).asInstanceOf[OrderExecuted]

          oe.submittedRemaining.avgWeighedPriceNominator shouldBe BigInteger.valueOf(sAvgWeighedPriceNominator)
          oe.submittedRemaining.fillingInfo.avgWeighedPrice shouldBe submittedAvgPrice

          oe.counterRemaining.avgWeighedPriceNominator shouldBe BigInteger.valueOf(cAvgWeighedPriceNominator)
          oe.counterRemaining.fillingInfo.avgWeighedPrice shouldBe counterAvgPrice
        }
      }

      buy.forMarket { buyMo =>
        withClue(s"$orderType submitted remaining:\n") {

          val remainingAmount = 30.waves
          val remainingFee    = 0.00045.waves
          val awpNominator    = BigInteger.valueOf(490.waves.usd)

          events(7) shouldBe a[OrderCanceled]
          events(7).asInstanceOf[OrderCanceled] should matchTo {
            OrderCanceled(buyMo.partial(remainingAmount, remainingFee, 10.usd, awpNominator), Events.OrderCanceledReason.BecameUnmatchable, now + 3)
          }
        }
      }
    }
  }

  private val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  private val asksGen: Gen[OrderBookSideSnapshot] = for {
    n      <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[SellLimitOrder]](n, sellLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  private val bidsGen: Gen[OrderBookSideSnapshot] = for {
    n      <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[BuyLimitOrder]](n, buyLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private lazy val lastTradeGen: Gen[LastTrade] = for {
    price     <- Gen.choose(1, Long.MaxValue)
    amount    <- Gen.choose(1, Long.MaxValue)
    orderType <- orderTypeGenerator
  } yield LastTrade(price, amount, orderType)

  private lazy val snapshotGen: Gen[OrderBookSnapshot] = for {
    asks      <- asksGen
    bids      <- bidsGen
    lastTrade <- Gen.option(lastTradeGen)
  } yield OrderBookSnapshot(bids, asks, lastTrade)

  private lazy val sideSnapshotSerGen: Gen[OrderBookSideSnapshot] = Gen.oneOf(asksGen, bidsGen)
}
