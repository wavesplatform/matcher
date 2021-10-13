package com.wavesplatform.dex.model

import java.nio.charset.StandardCharsets

import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.{Group, Monoid}
import cats.syntax.group._
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.OrderBook.OrderBookUpdates
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalacheck.Gen
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
  private val maxOrdersInLevel = 2

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
    newOrder <- newOrderGen
  } yield (askOrders, bidOrders, newOrder)

  "add" - {
    // TODO Move to OrderExecuted property test
    "OrderExecuted: submitted.spent == counter.receive && counter.spent == submitted.receive" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob = mkOrderBook(askOrders, bidOrders)
        val OrderBookUpdates(_, events, _, _) =
          ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee), (eventTs, _) => eventTs, 0L)
        val clue =
          s"""Events:
${formatEvents(events)}
"""

        withClue(clue) {
          events.foreach {
            case evt: Events.OrderExecuted =>
              val price = evt.counter.price

              val submittedSpent = spentPortfolio(evt.submitted, evt.executedAmount, price)
              val submittedReceive = receivePortfolio(evt.submitted, evt.executedAmount, price)

              val counterSpent = spentPortfolio(evt.counter, evt.executedAmount, price)
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
        val ob = mkOrderBook(askOrders, bidOrders)
        val balancesBefore = balancesBy(ob) |+| balancesBy(newOrder)
        val coinsBefore = Monoid.combineAll(balancesBefore.values)

        val OrderBookUpdates(updatedOb, events, _, _) =
          ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee), (eventTs, _) => eventTs, 0L)

        val balancesAfter = events.foldLeft(balancesBefore) {
          case (r, evt: Events.OrderExecuted) =>
            val price = evt.counter.price

            val submittedSpent = spentPortfolio(evt.submitted, evt.executedAmount, price)
            val submittedReceive = receivePortfolio(evt.submitted, evt.executedAmount, price)
            val submittedSpentFee = spentFee(evt.submitted, evt.executedAmount)

            val counterSpent = spentPortfolio(evt.counter, evt.executedAmount, price)
            val counterReceive = receivePortfolio(evt.counter, evt.executedAmount, price)
            val counterSpentFee = spentFee(evt.counter, evt.executedAmount)

            r |+|
            Monoid.combineAll(Seq(
              Map(evt.submitted.order.senderPublicKey -> submittedReceive),
              Map(evt.counter.order.senderPublicKey -> counterReceive),
              Map((matcher: PublicKey) -> submittedSpentFee),
              Map((matcher: PublicKey) -> counterSpentFee)
            )) |-|
            Monoid.combineAll(Seq(
              Map(evt.submitted.order.senderPublicKey -> submittedSpent),
              Map(evt.submitted.order.senderPublicKey -> submittedSpentFee),
              Map(evt.counter.order.senderPublicKey -> counterSpent),
              Map(evt.counter.order.senderPublicKey -> counterSpentFee)
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

    "order book invariant" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob = mkOrderBook(askOrders, bidOrders)
        val OrderBookUpdates(updatedOb, events, _, _) =
          ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee), (eventTs, _) => eventTs, 0L)

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
"""

        withClue(clue) {
          obAskBidIntersectionInvariant(ob)
          obAskBidIntersectionInvariant(updatedOb)
        }
    }

    "result.levelChanges contains diff for updatedOrderBook.level" in forAll(coinsInvariantPropGen) {
      case (askOrders, bidOrders, newOrder) =>
        val ob = mkOrderBook(askOrders, bidOrders)
        val snapshot = ob.aggregatedSnapshot

        val OrderBookUpdates(updatedOb, events, levelChanges, _) =
          ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee), (eventTs, _) => eventTs, 0L)
        val updatedSnapshot = updatedOb.aggregatedSnapshot

        val actualLevelChanges = filterNonEmpty(levelChanges)
        val expectedLevelChanges = filterNonEmpty(toLevelAmounts(updatedSnapshot) |-| toLevelAmounts(snapshot))

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

Expected level changes:
${format(expectedLevelChanges)}

Actual level changes:
${format(actualLevelChanges)}

Aggregated snapshot:
$snapshot

Aggregated updated snapshot:
$updatedSnapshot
"""

        withClue(clue) {
          withClue("asks: ") {
            expectedLevelChanges.asks should matchTo(actualLevelChanges.asks)
          }

          withClue("bids: ") {
            expectedLevelChanges.bids should matchTo(actualLevelChanges.bids)
          }
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

    "order book invariant" in test(removedGen) { (_, _, obAfter, _, _) =>
      obAskBidIntersectionInvariant(obAfter)
    }

    "no other order was removed" in test(removedGen) { (ob, orderIdToCancel, obAfter, _, _) =>
      val orderIdsBefore = orderIds(ob)
      val orderIdsAfter = orderIds(obAfter)
      (orderIdsBefore - orderIdToCancel) should matchTo(orderIdsAfter)
    }

    "only one level was changed" in test(removedGen) { (_, _, _, events, levelChanges) =>
      val r = (levelChanges.asks.isEmpty || levelChanges.bids.isEmpty) && (levelChanges.asks ++ levelChanges.bids).size == 1
      (events.isEmpty || r) shouldBe true
    }

    "result.levelChanges contains diff for updatedOrderBook.level" in test(removedGen) { (obBefore, _, obAfter, events, levelChanges) =>
      val snapshotBefore = obBefore.aggregatedSnapshot
      val snapshotAfter = obAfter.aggregatedSnapshot

      val actualLevelChanges = filterNonEmpty(levelChanges)
      val expectedLevelChanges = filterNonEmpty(toLevelAmounts(snapshotAfter) |-| toLevelAmounts(snapshotBefore))

      expectedLevelChanges should matchTo(actualLevelChanges)
    }

    def test(gen: Gen[(OrderBook, Order.Id)])(f: (OrderBook, Order.Id, OrderBook, Seq[Event], LevelAmounts) => Any): Unit = forAll(gen) {
      case (origOb, orderIdToCancel) =>
        val (updatedOb, events, levelChanges) = origOb.cancel(orderIdToCancel, Events.OrderCanceledReason.RequestExecuted, ts)
        withClue(mkClue(orderIdToCancel, origOb, updatedOb, events, levelChanges)) {
          f(origOb, orderIdToCancel, updatedOb, events.toSeq, levelChanges)
        }
    }

    def mkClue(orderIdToCancel: Order.Id, obBefore: OrderBook, obAfter: OrderBook, event: Option[Event], levelChanges: LevelAmounts): String =
      s"""
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

  "cancelAll" - {
    "canceled all orders" in forAll(cancelAllPropGen) {
      case (askOrders, bidOrders) =>
        val ob = mkOrderBook(askOrders, bidOrders)
        val obBefore = format(ob)
        val orderIdsBefore = orderIds(ob)

        val OrderBookUpdates(obAfter, events, _, _) = ob.cancelAll(ts, Events.OrderCanceledReason.RequestExecuted)
        val canceledOrders = events.collect { case evt: OrderCanceled => evt.acceptedOrder.order.id() }.toSet
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

    "result.levelChanges == -obBefore.snapshot" in forAll(cancelAllPropGen) {
      case (askOrders, bidOrders) =>
        val ob = mkOrderBook(askOrders, bidOrders)

        val OrderBookUpdates(_, _, levelChanges, _) = ob.cancelAll(ts, Events.OrderCanceledReason.RequestExecuted)
        val expectedLevelChanges = Group.inverse(toLevelAmounts(ob.aggregatedSnapshot))

        expectedLevelChanges should matchTo(levelChanges)
    }
  }

  private def obAskBidIntersectionInvariant(ob: OrderBook): Unit = {
    val firstAskPrice = ob.asks.headOption.map(_._1).getOrElse(Long.MaxValue)
    val firstBidPrice = ob.bids.headOption.map(_._1).getOrElse(Long.MinValue)
    firstAskPrice should be > firstBidPrice
  }

  private def orderIds(ob: OrderBook): Set[Order.Id] = ob.allOrders.map(_.order.id()).toSet
  private def hasOrder(ob: OrderBook, id: Order.Id): Boolean = ob.allOrders.exists(_.order.id() == id)

  private def balancesBy(ob: OrderBook): Map[PublicKey, Map[Asset, Long]] =
    ob.allOrders.foldLeft(Map.empty[PublicKey, Map[Asset, Long]]) {
      case (r, o) => r |+| balancesBy(o)
    }

  private def balancesBy(o: AcceptedOrder): Map[PublicKey, Map[Asset, Long]] = Map(o.order.senderPublicKey -> o.requiredBalance)

  private def spentPortfolio(ao: AcceptedOrder, executedAmount: Long, executedPrice: Long) =
    Map(ao.spentAsset -> ao.order.getSpendAmount(executedAmount, executedPrice).explicitGet())

  private def receivePortfolio(ao: AcceptedOrder, executedAmount: Long, executedPrice: Long) =
    Map(ao.rcvAsset -> ao.order.getReceiveAmount(executedAmount, executedPrice).explicitGet())

  private def spentFee(ao: AcceptedOrder, executedAmount: Long) =
    Map(ao.feeAsset -> AcceptedOrder.partialFee(ao.order.matcherFee, ao.order.amount, executedAmount))

  private def toLevelAmounts(s: OrderBookAggregatedSnapshot): LevelAmounts =
    new LevelAmounts(
      asks = s.asks.collect {
        case x if x.amount != 0 => x.price -> x.amount
      }.toMap,
      bids = s.bids.collect {
        case x if x.amount != 0 => x.price -> x.amount
      }.toMap
    )

  private def filterNonEmpty(x: LevelAmounts): LevelAmounts =
    new LevelAmounts(
      asks = x.asks.filter(_._2 != 0),
      bids = x.bids.filter(_._2 != 0)
    )

  private def formatSide(xs: Iterable[(Long, Level)]): String =
    xs.map { case (p, orders) => s"$p -> ${orders.map(format).mkString(", ")}" }.mkString("\n")

  private def formatEvents(xs: IterableOnce[Event]): String =
    xs.iterator.map(format).mkString("\n")

  private def format(x: LevelAmounts): String = s"""Asks: ${x.asks.mkString(", ")}
Bids: ${x.bids.mkString(", ")}"""

  private def format(x: OrderBook): String = s"""
Asks (rcv=${assetPair.priceAsset}, spt=${assetPair.amountAsset}):
${formatSide(x.asks)}

Bids (rcv=${assetPair.amountAsset}, spt=${assetPair.priceAsset}):
${formatSide(x.bids)}"""

  private def format(x: Event): String = x match {
    case x: OrderExecuted => s"executed(a=${x.executedAmount}, c=${format(x.counter)}, s=${format(x.submitted)})"
    case x: OrderAdded => s"added(${format(x.order)})"
    case x: OrderCanceled => s"canceled(${x.reason}, ${format(x.acceptedOrder)})"
  }

  private def format(x: AcceptedOrder): String = {
    val name = x match {
      case _: LimitOrder => "limit"
      case _: MarketOrder => "market"
    }

    s"""$name(a=${x.amount}, f=${x.fee}, ${format(x.order)}, rcv=${x.receiveAmount}, requiredBalance={ ${x.requiredBalance.mkString(", ")} })"""
  }

  private def format(x: Order): String = s"""Order(${x.idStr()}, ${x.orderType}, a=${x.amount}, p=${x.price}, f=${x.matcherFee} ${x.feeAsset})"""
}
