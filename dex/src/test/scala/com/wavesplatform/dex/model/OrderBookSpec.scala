package com.wavesplatform.dex.model

import java.nio.ByteBuffer

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.{Normalization, Price}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderExecuted}
import com.wavesplatform.dex.model.OrderBook.{LastTrade, Level, SideSnapshot, Snapshot}
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import com.wavesplatform.dex.settings.{MatchingRule, OrderFeeSettings}
import com.wavesplatform.dex.time.NTPTime
import com.wavesplatform.dex.{Matcher, MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.{SortedSet, mutable}

class OrderBookSpec
    extends AnyFreeSpec
    with PropertyChecks
    with Matchers
    with MatcherSpecBase
    with NTPTime
    with NoShrink
    with TableDrivenPropertyChecks {

  implicit class OrderBookOps(ob: OrderBook) {
    def append(ao: AcceptedOrder, ts: Long, tickSize: Long = MatchingRule.DefaultRule.tickSize): Seq[Event] =
      ob.add(ao, ts, (t, m) => m.matcherFee -> t.matcherFee, tickSize)
  }

  val pair: AssetPair = AssetPair(Waves, mkAssetId("BTC"))

  "place buy orders with different prices" in {
    val ord1 = LimitOrder(buy(pair, 1583290045643L, 34118))
    val ord2 = LimitOrder(buy(pair, 170484969L, 34120))
    val ord3 = LimitOrder(buy(pair, 44521418496L, 34000))

    val ob = OrderBook.empty()
    ob.append(ord1, ntpNow)
    ob.append(ord2, ntpNow)
    ob.append(ord3, ntpNow)

    ob.allOrders.toSeq.map(_._2) shouldEqual Seq(ord2, ord1, ord3)
  }

  "place several buy orders at the same price" in {}

  "place orders with different tick sizes" in {
    val ob = OrderBook.empty()

    val normalizedTickSizes =
      Array(1L, 7L, 8L, 100L, 211L, 320L, 100000L, 124337L, 250250L, toNormalized(10L), 651983183L, toNormalized(100L))
    val prices = Gen.choose(1, 100000L)
    for (tickSize <- normalizedTickSizes) {
      forAll(prices) { price =>
        ob.append(LimitOrder(buy(pair, 1583290045643L, price)), ntpNow, tickSize = tickSize)
      }
      forAll(prices) { price =>
        ob.append(LimitOrder(sell(pair, 984651354686L, price)), ntpNow, tickSize = tickSize)
      }
      for ((level, orders) <- ob.getBids; order <- orders) {
        order.price - level should be < tickSize
        level % tickSize shouldBe 0
      }
      for ((level, orders) <- ob.getAsks; order <- orders) {
        level - order.price should be < tickSize
        level % tickSize shouldBe 0
      }
      ob.cancelAll(ntpNow)
    }
  }

  "place buy and sell orders with prices different from each other less than tick size in one level" in {
    val ob                 = OrderBook.empty()
    val tickSize: Long     = 100L
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

    ob.append(buyOrd1, ntpNow, tickSize = normalizedTickSize)
    ob.append(buyOrd2, ntpNow, tickSize = normalizedTickSize)
    ob.append(buyOrd3, ntpNow, tickSize = normalizedTickSize)
    ob.append(buyOrd4, ntpNow, tickSize = normalizedTickSize)
    ob.append(buyOrd5, ntpNow, tickSize = normalizedTickSize)
    ob.append(buyOrd6, ntpNow, tickSize = normalizedTickSize)
    ob.append(buyOrdZero, ntpNow, tickSize = normalizedTickSize)

    ob.append(sellOrd1, ntpNow, tickSize = normalizedTickSize)
    ob.append(sellOrd2, ntpNow, tickSize = normalizedTickSize)
    ob.append(sellOrd3, ntpNow, tickSize = normalizedTickSize)
    ob.append(sellOrd4, ntpNow, tickSize = normalizedTickSize)
    ob.append(sellOrd5, ntpNow, tickSize = normalizedTickSize)
    ob.append(sellOrd6, ntpNow, tickSize = normalizedTickSize)

    ob.getBids.keySet shouldBe SortedSet[Long](0, 34300, 34200, 34100).map(toNormalized)
    ob.getAsks.keySet shouldBe SortedSet[Long](44100, 44200, 44300, 44400).map(toNormalized)

    ob.getBids shouldBe mutable.TreeMap[Price, Level](
      Seq(
        0     -> Vector(buyOrdZero),
        34300 -> Vector(buyOrd4, buyOrd5, buyOrd6),
        34200 -> Vector(buyOrd3),
        34100 -> Vector(buyOrd1, buyOrd2)
      ).map { case (price, orders) => toNormalized(price) -> orders }: _*
    )

    ob.getAsks shouldBe mutable.TreeMap[Price, Level](
      Seq(
        44100 -> Vector(sellOrd1),
        44200 -> Vector(sellOrd2),
        44300 -> Vector(sellOrd3),
        44400 -> Vector(sellOrd4, sellOrd5, sellOrd6)
      ).map { case (price, orders) => toNormalized(price) -> orders }: _*
    )

    ob.cancelAll(ntpNow)
    val sellTickSizeOrder = sell(pair, 54521418493L, 40)

    ob.append(LimitOrder(sellTickSizeOrder), ntpNow, tickSize = normalizedTickSize)

    ob.getAsks shouldBe mutable.TreeMap[Price, Level](
      Seq(
        tickSize -> Vector(sellTickSizeOrder)
      ).map { case (price, orders) => toNormalized(price) -> orders.map(LimitOrder.apply) }: _*
    )
  }

  "place matchable orders with and without tick size" in {
    val ob = OrderBook.empty()

    val amt                = 54521418493L
    val normalizedTickSize = toNormalized(8)

    val counterSellOrder  = LimitOrder(sell(pair, amt, 9))
    val submittedBuyOrder = LimitOrder(buy(pair, amt, 10))

    val counterOrderTime   = ntpNow
    val submittedOrderTime = ntpNow

    withClue("matchable orders should be matched without tick size:\n") {
      ob.append(counterSellOrder, counterOrderTime) shouldBe Seq(OrderAdded(counterSellOrder, counterOrderTime))
      ob.append(submittedBuyOrder, submittedOrderTime) shouldBe Seq(
        OrderExecuted(submittedBuyOrder, counterSellOrder, submittedOrderTime, submittedBuyOrder.matcherFee, counterSellOrder.matcherFee)
      )

      ob.getAsks shouldBe empty
      ob.getBids shouldBe empty
    }

    withClue("matchable orders should not be matched with tick size:\n") {
      ob.append(counterSellOrder, ntpNow, tickSize = normalizedTickSize)
      ob.append(submittedBuyOrder, ntpNow, tickSize = normalizedTickSize)

      ob.getAsks shouldBe mutable.TreeMap[Price, Level](
        Seq(16 -> Vector(counterSellOrder)).map { case (price, orders) => toNormalized(price) -> orders }: _*
      )

      ob.getBids shouldBe mutable.TreeMap[Price, Level](
        Seq(8 -> Vector(submittedBuyOrder)).map { case (price, orders) => toNormalized(price) -> orders }: _*
      )
    }
  }

  "old counter orders should be matched with the new ones (with new activated tick-size)" in {

    val ob = OrderBook.empty()

    def normalizedTickSize(tickSize: Double): Price = Normalization.normalizePrice(tickSize, 8, 2)

    val counter   = LimitOrder(createOrder(wavesUsdPair, SELL, 100.waves, 3.15))
    val submitted = LimitOrder(createOrder(wavesUsdPair, BUY, 100.waves, 3.15))

    val counterTs   = counter.order.timestamp
    val submittedTs = submitted.order.timestamp

    withClue("Counter SELL order (price = 3.15, tick size disabled) and submitted BUY order (price = 3.15, tick size = 0.1) should be matched:\n") {

      ob.append(counter, counterTs) shouldBe Seq(OrderAdded(counter, counterTs))
      ob.append(submitted, submittedTs, tickSize = normalizedTickSize(0.1)) shouldBe Seq(
        OrderExecuted(submitted, counter, submittedTs, submitted.matcherFee, counter.matcherFee)
      )

      ob.getAsks shouldBe empty
      ob.getBids shouldBe empty
    }
  }

  "place sell orders" in {
    val ord1 = sell(pair, 1583290045643L, 34110)
    val ord2 = sell(pair, 170484969L, 34220)
    val ord3 = sell(pair, 44521418496L, 34000)

    Seq(ord3, ord1, ord2).map(LimitOrder(_))
  }

  "place several sell orders at the same price" - {
    "with same level" - {
      "TickSize.Enabled" in {
        val normalizedTickSize = toNormalized(100L)
        val ob                 = OrderBook.empty()

        val sellOrder = LimitOrder(sell(pair, 54521418493L, 44389))
        ob.append(sellOrder, ntpNow, tickSize = normalizedTickSize)
        ob.append(sellOrder, ntpNow, tickSize = normalizedTickSize)

        ob.getAsks.size shouldBe 1
        ob.getAsks.head._2.toList shouldBe List(sellOrder, sellOrder)
      }

      "MatchingRules.Default.normalizedTickSize" in {
        val ob = OrderBook.empty()

        val sellOrder = LimitOrder(sell(pair, 54521418493L, 44389))
        ob.append(sellOrder, ntpNow, tickSize = MatchingRule.DefaultRule.tickSize)
        ob.append(sellOrder, ntpNow, tickSize = MatchingRule.DefaultRule.tickSize)

        ob.getAsks.size shouldBe 1
        ob.getAsks.head._2.toList shouldBe List(sellOrder, sellOrder)
      }
    }

    "with different levels" in {
      val ob = OrderBook.empty()

      val sellOrder = LimitOrder(sell(pair, 54521418493L, 44389))
      ob.append(sellOrder, ntpNow, tickSize = toNormalized(100L))
      ob.append(sellOrder, ntpNow, tickSize = MatchingRule.DefaultRule.tickSize)

      ob.getAsks.size shouldBe 2

      ob.getAsks.head._2.head shouldBe sellOrder
      ob.getAsks.last._2.head shouldBe sellOrder
    }
  }

  "sell market" in {
    val ord1 = buy(pair, 10 * Order.PriceConstant, 100)
    val ord2 = buy(pair, 10 * Order.PriceConstant, 105)

    val ob = OrderBook.empty()
    ob.append(LimitOrder(ord1), ntpNow)
    ob.append(LimitOrder(ord2), ntpNow)

    ob.allOrders.map(_._2) shouldEqual Seq(BuyLimitOrder(ord2.amount, ord2.matcherFee, ord2), BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1))

    val ord3 = sell(pair, 10 * Order.PriceConstant, 100)
    ob.append(LimitOrder(ord3), ntpNow)

    ob.allOrders.map(_._2) shouldEqual Seq(BuyLimitOrder(ord1.amount, ord1.matcherFee, ord1))
  }

  "execute orders at different price levels" in {
    val ord1 = sell(pair, 10 * Order.PriceConstant, 100)
    val ord2 = sell(pair, 5 * Order.PriceConstant, 110)
    val ord3 = sell(pair, 10 * Order.PriceConstant, 110)
    val ord4 = buy(pair, 22 * Order.PriceConstant, 115)

    val ob = OrderBook.empty()
    ob.append(LimitOrder(ord1), ntpNow)
    ob.append(LimitOrder(ord2), ntpNow)
    ob.append(LimitOrder(ord3), ntpNow)
    ob.append(LimitOrder(ord4), ntpNow)

    val restAmount = ord1.amount + ord2.amount + ord3.amount - ord4.amount

    ob.allOrders.map(_._2) shouldEqual Seq(
      SellLimitOrder(
        restAmount,
        ord3.matcherFee - AcceptedOrder.partialFee(ord3.matcherFee, ord3.amount, ord3.amount - restAmount),
        ord3
      ))
  }

  "partially execute order with small remaining part" in {
    val ord1 = sell(pair, 200000000, 0.00041)
    val ord2 = sell(pair, 100000000, 0.0004)
    val ord3 = buy(pair, 100000001, 0.00045)

    val ob = OrderBook.empty()

    ob.append(LimitOrder(ord1), ntpNow)
    ob.append(LimitOrder(ord2), ntpNow)
    ob.append(LimitOrder(ord3), ntpNow)

    ob.allOrders.map(_._2) shouldEqual Seq(SellLimitOrder(ord1.amount, ord1.matcherFee, ord1))
  }

  "partially execute order with zero fee remaining part" in {
    val ord1 = sell(pair, 1500.waves, 0.0006999)
    val ord2 = sell(pair, 3075248828L, 0.00067634)
    val ord3 = buy(pair, 3075363900L, 0.00073697)

    val ob = OrderBook.empty()

    ob.append(LimitOrder(ord1), ntpNow)
    ob.append(LimitOrder(ord2), ntpNow)
    ob.append(LimitOrder(ord3), ntpNow)

    val corrected1 = Order.correctAmount(ord2.amount, ord2.price)
    val leftovers1 = ord3.amount - corrected1
    val corrected2 = Order.correctAmount(leftovers1, ord1.price)
    val restAmount = ord1.amount - corrected2
    // See OrderExecuted.submittedRemainingFee
    val restFee = ord1.matcherFee - AcceptedOrder.partialFee(ord1.matcherFee, ord1.amount, corrected2)
    ob.allOrders.toSeq.map(_._2) shouldEqual Seq(SellLimitOrder(restAmount, restFee, ord1))
  }

  "partially execute order with price > 1 and zero fee remaining part " in {
    val pair = AssetPair(IssuedAsset(ByteStr("BTC".getBytes)), IssuedAsset(ByteStr("USD".getBytes)))
    val ord1 = sell(pair, 0.1.waves, 1850)
    val ord2 = sell(pair, 0.01.waves, 1840)
    val ord3 = buy(pair, 0.0100001.waves, 2000)

    val ob = OrderBook.empty()

    ob.append(LimitOrder(ord1), ntpNow)
    ob.append(LimitOrder(ord2), ntpNow)
    ob.append(LimitOrder(ord3), ntpNow)

    val restAmount = ord1.amount - (ord3.amount - ord2.amount)
    val restFee    = ord1.matcherFee - AcceptedOrder.partialFee(ord1.matcherFee, ord1.amount, ord3.amount - ord2.amount)
    ob.allOrders.toSeq.map(_._2) shouldEqual Seq(SellLimitOrder(restAmount, restFee, ord1))
  }

  "buy small amount of pricey asset" in {
    val p = AssetPair(IssuedAsset(ByteStr("WAVES".getBytes)), IssuedAsset(ByteStr("USD".getBytes)))
    val b = rawBuy(p, 700000L, 280)
    val s = rawSell(p, 30000000000L, 280)

    val ob = OrderBook.empty()
    ob.append(LimitOrder(s), ntpNow)
    ob.append(LimitOrder(b), ntpNow)

    val restSAmount = Order.correctAmount(700000L, 280)
    val restAmount  = 30000000000L - restSAmount
    val restFee     = s.matcherFee - AcceptedOrder.partialFee(s.matcherFee, s.amount, restSAmount)
    ob.allOrders.map(_._2) shouldEqual Seq(SellLimitOrder(restAmount, restFee, s))
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
    val dest = new mutable.ArrayBuilder.ofByte
    SideSnapshot.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    SideSnapshot.loFromBytes(bb) shouldBe x
  }

  "SideSnapshot serialization" in forAll(sideSnapshotSerGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    SideSnapshot.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    SideSnapshot.fromBytes(bb) shouldBe x
  }

  "LastTrade serialization" in forAll(lastTradeGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    LastTrade.serialize(dest, x)
    val bb = ByteBuffer.wrap(dest.result())
    LastTrade.fromBytes(bb) shouldBe x
  }

  "Snapshot serialization" in forAll(snapshotGen) { x =>
    val dest = new mutable.ArrayBuilder.ofByte
    Snapshot.serialize(dest, x)
    val bb       = ByteBuffer.wrap(dest.result())
    val restored = Snapshot.fromBytes(bb)
    restored.asks shouldBe x.asks
    restored.bids shouldBe x.bids
    restored.lastTrade shouldBe x.lastTrade
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
      val ob                  = OrderBook.empty()
      val normalizedOrderTFee = Normalization.normalizeAmountAndFee(orderTFee, 8)

      val maker = limit(mAmt, SELL, orderMFee)
      val taker = if (isTMarket) market(tAmt, BUY, normalizedOrderTFee, tFeeAsset) else limit(tAmt, BUY, normalizedOrderTFee, tFeeAsset)
      val gmtf  = Matcher.getMakerTakerFee(ofs)(_, _)
      val evt   = { ob.add(maker, System.currentTimeMillis, gmtf); ob.add(taker, System.currentTimeMillis, gmtf) }.head

      evt shouldBe a[OrderExecuted]
      val oe = evt.asInstanceOf[OrderExecuted]
      oe.counterExecutedFee shouldBe eMFee
      oe.submittedExecutedFee shouldBe eTFee
    }
  }

  private val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  private val asksGen: Gen[SideSnapshot] = for {
    n      <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[SellLimitOrder]](n, sellLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  private val bidsGen: Gen[SideSnapshot] = for {
    n      <- Gen.choose(0, 10)
    levels <- Gen.containerOfN[Vector, Vector[BuyLimitOrder]](n, buyLevelGen)
    prices <- Gen.containerOfN[Vector, Long](n, Gen.choose(1, 1000L))
  } yield prices.zip(levels).toMap

  private val lastTradeGen: Gen[LastTrade] = for {
    price     <- Gen.choose(1, Long.MaxValue)
    amount    <- Gen.choose(1, Long.MaxValue)
    orderType <- orderTypeGenerator
  } yield LastTrade(price, amount, orderType)

  private val snapshotGen: Gen[Snapshot] = for {
    asks      <- asksGen
    bids      <- bidsGen
    lastTrade <- Gen.option(lastTradeGen)
  } yield Snapshot(bids, asks, lastTrade)

  private val sideSnapshotSerGen: Gen[SideSnapshot] = Gen.oneOf(asksGen, bidsGen)
}
