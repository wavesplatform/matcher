package com.wavesplatform.dex.model

import java.nio.charset.StandardCharsets

import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.Monoid
import cats.syntax.group._
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalacheck.Gen
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OrderBookSpec
    extends AnyFreeSpecLike
    with OrderBookGen
    with DiffMatcherWithImplicits
    with Matchers
    with ScalaCheckPropertyChecks
    with NoShrink {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 1000)

  private val maxLevelsInOrderBook = 6
  private val maxOrdersInLevel     = 2

  // TODO migrate to long ranges in 2.13
  private val askPricesMin = 1000L * Order.PriceConstant
  private val askPricesMax = 2000L * Order.PriceConstant
  private val askPricesGen = Gen.choose(askPricesMin, askPricesMax)

  private val bidPricesMin = 1L * Order.PriceConstant
  private val bidPricesMax = 999L * Order.PriceConstant
  private val bidPricesGen = Gen.choose(bidPricesMin, bidPricesMax)

  private val allPricesGen = Gen.choose(bidPricesMin, askPricesMax)

  private val newOrderGen: Gen[AcceptedOrder] = orderSideGen.flatMap { x =>
    Gen.oneOf(
      limitOrderGen(orderGen(allPricesGen, x)),
      marketOrderGen(orderGen(allPricesGen, x))
    )
  }

  private val coinsInvariantPropGen = for {
    (askOrders, bidOrders) <- flexibleSidesOrdersGen(maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen, bidPricesGen)
    newOrder               <- newOrderGen
  } yield (askOrders, bidOrders, newOrder)

  "add" - {
    // TODO Move to OrderExecuted property test
    "OrderExecuted: submitted.spent == counter.receive && counter.spent == submitted.receive" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob             = mkOrderBook(askOrders, bidOrders)
        val (_, events, _) = ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee))
        val clue =
          s"""Events:
${formatEvents(events)}
"""

        withClue(clue) {
          events.foreach {
            case evt: Events.OrderExecuted =>
              val price = evt.counter.price

              val submittedSpent   = spentPortfolio(evt.submitted, evt.executedAmount, price)
              val submittedReceive = receivePortfolio(evt.submitted, evt.executedAmount, price)

              val counterSpent   = spentPortfolio(evt.counter, evt.executedAmount, price)
              val counterReceive = receivePortfolio(evt.counter, evt.executedAmount, price)

              withClue(s"$evt: submitted.spent == counter.receive: ") {
                submittedSpent should matchTo(counterReceive)
              }

              withClue(s"$evt: counter.spent == submitted.receive: ") {
                counterSpent should matchTo(submittedReceive)
              }

            case _ =>
          }
        }
    }

    "coins invariant" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob             = mkOrderBook(askOrders, bidOrders)
        val balancesBefore = balancesBy(ob) |+| balancesBy(newOrder)
        val coinsBefore    = Monoid.combineAll(balancesBefore.values)

        val (updatedOb, events, _) = ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee))

        val balancesAfter = events.foldLeft(balancesBefore) {
          case (r, evt: Events.OrderExecuted) =>
            val price = evt.counter.price

            val submittedSpent    = spentPortfolio(evt.submitted, evt.executedAmount, price)
            val submittedReceive  = receivePortfolio(evt.submitted, evt.executedAmount, price)
            val submittedSpentFee = spentFee(evt.submitted, evt.executedAmount)

            val counterSpent    = spentPortfolio(evt.counter, evt.executedAmount, price)
            val counterReceive  = receivePortfolio(evt.counter, evt.executedAmount, price)
            val counterSpentFee = spentFee(evt.counter, evt.executedAmount)

            r |+|
              Monoid.combineAll(Seq(
                Map(evt.submitted.order.senderPublicKey -> submittedReceive),
                Map(evt.counter.order.senderPublicKey   -> counterReceive),
                Map((matcher: PublicKey)                -> submittedSpentFee),
                Map((matcher: PublicKey)                -> counterSpentFee)
              )) |-|
              Monoid.combineAll(Seq(
                Map(evt.submitted.order.senderPublicKey -> submittedSpent),
                Map(evt.submitted.order.senderPublicKey -> submittedSpentFee),
                Map(evt.counter.order.senderPublicKey   -> counterSpent),
                Map(evt.counter.order.senderPublicKey   -> counterSpentFee),
              ))

          case (r, _) => r
        }

        val coinsAfter = Monoid.combineAll(balancesAfter.values)

        val diff = coinsAfter |-| coinsBefore
        val clue =
          s"""
Pair:
$assetPair

Order:
${format(newOrder)}

OrderBook before:
${format(ob)}

OrderBook after:
${format(updatedOb)}

Events:
${formatEvents(events)}

Diff:
${diff.mkString("\n")}
"""

        withClue(clue) {
          coinsBefore should matchTo(coinsAfter)
        }
    }

    "updatedOrderBook.level contains result.levelChanges" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob                                = mkOrderBook(askOrders, bidOrders)
        val (updatedOb, events, levelChanges) = ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee))

        val snapshot = updatedOb.aggregatedSnapshot
        def getSnapshotLevel(tpe: OrderType, levelPrice: Long): Long =
          tpe.askBid(snapshot.asks, snapshot.bids).collectFirst { case x: LevelAgg if x.price == levelPrice => x.amount }.getOrElse(0L)

        val filteredAsks = levelChanges.asks.map { case (price, _) => price -> getSnapshotLevel(OrderType.SELL, price) }
        val filteredBids = levelChanges.bids.map { case (price, _) => price -> getSnapshotLevel(OrderType.BUY, price) }

        val clue =
          s"""
Pair:
$assetPair

Order:
${format(newOrder)}

OrderBook before:
${format(ob)}

OrderBook after:
${format(updatedOb)}

Events:
${formatEvents(events)}

Level changes:
${format(levelChanges)}

Aggregated snapshot:
${updatedOb.aggregatedSnapshot}
"""

        withClue(clue) {
          withClue("asks: ") {
            filteredAsks should matchTo(levelChanges.asks)
          }

          withClue("bids: ") {
            filteredBids should matchTo(levelChanges.bids)
          }
        }
    }

    "result.levelChanges.prices == all(event.executedPrice)" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob                                = mkOrderBook(askOrders, bidOrders)
        val (updatedOb, events, levelChanges) = ob.add(newOrder, ts, _.matcherFee -> _.matcherFee)

        val levelChangesPrices = levelChanges.asks.keySet ++ levelChanges.bids.keySet

        // tickSize == 1
        val eventPrices = events
          .collect {
            case evt: OrderAdded    => evt.order.price
            case evt: OrderExecuted => evt.counter.price
            case evt: OrderCanceled => evt.acceptedOrder.price // Ignore market orders, because the are canceled at end
          }
          .filter(p => ob.hasPrice(p) || updatedOb.hasPrice(p))
          .toSet

        val clue =
          s"""
Order:
${format(newOrder)}

Events:
${formatEvents(events)}

Level changes:
${format(levelChanges)}

Level changes prices:
${levelChangesPrices.toVector.sorted.mkString("\n")}

Event prices:
${eventPrices.toVector.sorted.mkString("\n")}
"""

        withClue(clue) {
          levelChangesPrices.toList.sorted should matchTo(eventPrices.toList.sorted)
        }
    }
  }

  "cancel" - {
    val orderIdGen = Gen.alphaNumStr.suchThat(_.nonEmpty).map(x => ByteStr(x.getBytes(StandardCharsets.UTF_8)))
    val removedGen = for {
      (askOrders, bidOrders) <- flexibleSidesOrdersGen(maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen, bidPricesGen)
      orderIdToCancel <- {
        if (askOrders.isEmpty && bidOrders.isEmpty) orderIdGen
        else Gen.oneOf((askOrders ++ bidOrders).map(_.order.id()))
      }
    } yield (mkOrderBook(askOrders, bidOrders), orderIdToCancel)

    "order is removed from the OrderBook" in test(removedGen) { (_, orderIdToCancel, obAfter, _, _) =>
      val orderRemoved = !hasOrder(obAfter, orderIdToCancel)
      orderRemoved shouldBe true
    }

    "no other order was removed" in test(removedGen) { (ob, orderIdToCancel, obAfter, _, _) =>
      val orderIdsBefore = orderIds(ob)
      val orderIdsAfter  = orderIds(obAfter)
      (orderIdsBefore - orderIdToCancel) should matchTo(orderIdsAfter)
    }

    "only one level was changed" in test(removedGen) { (_, _, _, events, levelChanges) =>
      val r = (levelChanges.asks.isEmpty || levelChanges.bids.isEmpty) && (levelChanges.asks ++ levelChanges.bids).size == 1
      (events.isEmpty || r) shouldBe true
    }

    "levelChanges.levelAmount == obAfter.levelAmount" in test(removedGen) { (_, _, obAfter, events, levelChanges) =>
      lazy val r = {
        val (price, amountFromChanges) = (levelChanges.asks ++ levelChanges.bids).take(1).toList.head
        val amountFromOb               = obAfter.asks.get(price).orElse(obAfter.bids.get(price)).fold(0L)(_.map(_.amount).sum)
        amountFromChanges == amountFromOb
      }
      (events.isEmpty || r) shouldBe true
    }

    def test(gen: Gen[(OrderBook, Order.Id)])(f: (OrderBook, Order.Id, OrderBook, Seq[Event], LevelAmounts) => Assertion): Unit = forAll(gen) {
      case (origOb, orderIdToCancel) =>
        val (updatedOb, events, levelChanges) = origOb.cancel(orderIdToCancel, ts)
        withClue(mkClue(orderIdToCancel, origOb, updatedOb, events, levelChanges)) {
          f(origOb, orderIdToCancel, updatedOb, events.toSeq, levelChanges)
        }
    }

    def mkClue(orderIdToCancel: Order.Id, obBefore: OrderBook, obAfter: OrderBook, event: Option[Event], levelChanges: LevelAmounts): String = s"""
Order id to cancel: $orderIdToCancel

OrderBook before:
${format(obBefore)}

OrderBook after:
${format(obAfter)}

Events:
${formatEvents(event)}

Level changes:
$levelChanges
"""
  }

  private val cancelAllPropGen = flexibleSidesOrdersGen(maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen, bidPricesGen)

  "cancelAll" in forAll(cancelAllPropGen) {
    case (askOrders, bidOrders) =>
      val ob             = mkOrderBook(askOrders, bidOrders)
      val obBefore       = format(ob)
      val orderIdsBefore = orderIds(ob)

      val (obAfter, events, _) = ob.cancelAll(ts)
      val canceledOrders       = events.collect { case evt: OrderCanceled => evt.acceptedOrder.order.id() }.toSet
      val clue =
        s"""
OrderBook before:
$obBefore

OrderBook after:
${format(ob)}

Events:
${formatEvents(events)}

Canceled orders:
${canceledOrders.mkString("\n")}
"""

      val orderIdsAfter = orderIds(obAfter)
      withClue(clue) {
        orderIdsAfter shouldBe empty
        events should have size canceledOrders.size
        events should have size orderIdsBefore.size
        canceledOrders should matchTo(orderIdsBefore)
        orderIdsAfter shouldBe empty
      }
  }

  private def orderIds(ob: OrderBook): Set[Order.Id]         = ob.allOrders.map(_.order.id()).toSet
  private def hasOrder(ob: OrderBook, id: Order.Id): Boolean = ob.allOrders.exists(_.order.id() == id)

  private def balancesBy(ob: OrderBook): Map[PublicKey, Map[Asset, Long]] =
    ob.allOrders.foldLeft(Map.empty[PublicKey, Map[Asset, Long]]) {
      case (r, o) => r |+| balancesBy(o)
    }

  private def balancesBy(o: AcceptedOrder): Map[PublicKey, Map[Asset, Long]] = Map(o.order.senderPublicKey -> o.requiredBalance)

  private def spentPortfolio(ao: AcceptedOrder, executedAmount: Long, executedPrice: Long) =
    Map(ao.spentAsset -> ao.order.getSpendAmount(executedAmount, executedPrice).explicitGet)

  private def receivePortfolio(ao: AcceptedOrder, executedAmount: Long, executedPrice: Long) =
    Map(ao.rcvAsset -> ao.order.getReceiveAmount(executedAmount, executedPrice).explicitGet)

  private def spentFee(ao: AcceptedOrder, executedAmount: Long) =
    Map(ao.feeAsset -> AcceptedOrder.partialFee(ao.order.matcherFee, ao.order.amount, executedAmount))

  private def formatSide(xs: Iterable[(Long, Level)]): String =
    xs.map { case (p, orders) => s"$p -> ${orders.map(format).mkString(", ")}" }.mkString("\n")

  private def formatEvents(xs: TraversableOnce[Event]): String =
    xs.map(format).mkString("\n")

  private def format(x: LevelAmounts): String = s"""Asks: ${x.asks.mkString(", ")}
Bids: ${x.bids.mkString(", ")}"""

  private def format(x: OrderBook): String = s"""
Asks (rcv=${assetPair.priceAsset}, spt=${assetPair.amountAsset}):
${formatSide(x.asks)}

Bids (rcv=${assetPair.amountAsset}, spt=${assetPair.priceAsset}):
${formatSide(x.bids)}"""

  private def format(x: Event): String = x match {
    case x: OrderExecuted => s"executed(a=${x.executedAmount}, c=${format(x.counter)}, s=${format(x.submitted)})"
    case x: OrderAdded    => s"added(${format(x.order)})"
    case x: OrderCanceled => s"canceled(s=${x.isSystemCancel}, ${format(x.acceptedOrder)})"
  }

  private def format(x: AcceptedOrder): String = {
    val name = x match {
      case _: LimitOrder  => "limit"
      case _: MarketOrder => "market"
    }

    s"""$name(a=${x.amount}, f=${x.fee}, ${format(x.order)}, rcv=${x.receiveAmount}, requiredBalance={ ${x.requiredBalance.mkString(", ")} })"""
  }

  private def format(x: Order): String = s"""Order(${x.idStr()}, ${x.orderType}, a=${x.amount}, p=${x.price}, f=${x.matcherFee} ${x.feeAsset})"""

  private implicit class OrderBookOps(val self: OrderBook) {
    def hasPrice(x: Price): Boolean = self.asks.contains(x) || self.bids.contains(x)
  }
}
