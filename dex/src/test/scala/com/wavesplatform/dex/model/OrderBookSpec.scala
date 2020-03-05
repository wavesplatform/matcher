package com.wavesplatform.dex.model

import java.nio.charset.StandardCharsets

import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.Monoid
import cats.syntax.group._
import com.wavesplatform.dex.NoShrink
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.gen.OrderBookGen
import com.wavesplatform.dex.model.Events.OrderCanceled
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

  "coins invariant" in forAll(coinsInvariantPropGen) {
    case (askOrders, bidOrders, newOrder) =>
      val ob             = mkOrderBook(askOrders, bidOrders)
      val obBefore       = format(ob)
      val balancesBefore = balancesBy(ob) |+| balancesBy(newOrder)
      val coinsBefore    = Monoid.combineAll(balancesBefore.values)

      val (obAfter, events) = ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee))

      val balancesAfter = events.foldLeft(balancesBefore) {
        case (r, evt: Events.OrderExecuted) =>
          val price            = evt.counter.price
          val submittedSpent   = Map(evt.submitted.spentAsset -> evt.submitted.order.getSpendAmount(evt.executedAmount, price).right.get)
          val submittedReceive = Map(evt.submitted.rcvAsset -> evt.submitted.order.getReceiveAmount(evt.executedAmount, price).right.get)
          val submittedSpentFee =
            Map(evt.submitted.feeAsset -> AcceptedOrder.partialFee(evt.submitted.order.matcherFee, evt.submitted.order.amount, evt.executedAmount))

          val counterSpent   = Map(evt.counter.spentAsset -> evt.counter.order.getSpendAmount(evt.executedAmount, price).right.get)
          val counterReceive = Map(evt.counter.rcvAsset   -> evt.counter.order.getReceiveAmount(evt.executedAmount, price).right.get)
          val counterSpentFee =
            Map(evt.counter.feeAsset -> AcceptedOrder.partialFee(evt.counter.order.matcherFee, evt.counter.order.amount, evt.executedAmount))

          withClue(s"$evt: submitted.spent == counter.receive: ") {
            submittedSpent should matchTo(counterReceive)
          }

          withClue(s"$evt: counter.spent == submitted.receive: ") {
            counterSpent should matchTo(submittedReceive)
          }

          r |+|
            Monoid.combineAll(Seq(
              Map(evt.submitted.order.senderPublicKey -> submittedReceive),
              Map(evt.counter.order.senderPublicKey   -> counterReceive),
              Map((matcher: PublicKey)                -> submittedSpentFee),
              Map((matcher: PublicKey)                -> counterSpentFee)
            )) |-|
            Monoid.combineAll(
              Seq(
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
        s"""Coins invariant
Pair:
$assetPair

Order:
${format(newOrder)}

OrderBook before:
$obBefore

OrderBook after:
${format(ob)}

Events:
${events.mkString("\n")}

Diff:
${diff.mkString("\n")}
"""

      withClue(clue) {
        coinsBefore should matchTo(coinsAfter)
      }
  }

  private val orderIdGen = Gen.alphaNumStr.suchThat(_.nonEmpty).map(x => ByteStr(x.getBytes(StandardCharsets.UTF_8)))
  private val cancelPropGen = for {
    (askOrders, bidOrders) <- flexibleSidesOrdersGen(maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen, bidPricesGen)
    orderIdToCancel <- {
      if (askOrders.isEmpty && bidOrders.isEmpty) orderIdGen
      else Gen.oneOf(orderIdGen, Gen.oneOf((askOrders ++ bidOrders).map(_.order.id())))
    }
  } yield (askOrders, bidOrders, orderIdToCancel)

  "cancel" in forAll(cancelPropGen) {
    case (askOrders, bidOrders, orderIdToCancel) =>
      val ob             = mkOrderBook(askOrders, bidOrders)
      val obBefore       = format(ob)
      val hadOrder       = hasOrder(ob, orderIdToCancel)
      val orderIdsBefore = orderIds(ob)

      val (obAfter, events) = ob.cancel(orderIdToCancel, ts)
      val clue =
        s"""
Order id to cancel: $orderIdToCancel
Had order: $hadOrder

OrderBook before:
$obBefore

OrderBook after:
${format(ob)}

Events:
${events.mkString("\n")}
"""

      val orderIdsAfter = orderIds(obAfter)
      withClue(clue) {
        if (hadOrder) {
          val orderRemoved = events.nonEmpty && !hasOrder(obAfter, orderIdToCancel)
          orderRemoved shouldBe true
        } else {
          events.isEmpty shouldBe true
        }

        withClue("no other order was removed: ") {
          (orderIdsBefore - orderIdToCancel) should matchTo(orderIdsAfter)
        }
      }
  }

  private val cancelAllPropGen = flexibleSidesOrdersGen(maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen, bidPricesGen)

  "cancelAll" in forAll(cancelAllPropGen) {
    case (askOrders, bidOrders) =>
      val ob             = mkOrderBook(askOrders, bidOrders)
      val obBefore       = format(ob)
      val orderIdsBefore = orderIds(ob)

      val (obAfter, events) = ob.cancelAll(ts)
      val canceledOrders    = events.collect { case evt: OrderCanceled => evt.acceptedOrder.order.id() }.toSet
      val clue =
        s"""
OrderBook before:
$obBefore

OrderBook after:
${format(ob)}

Events:
${events.mkString("\n")}

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

  private def formatSide(xs: Iterable[(Long, Level)]): String =
    xs.map { case (p, orders) => s"$p -> ${orders.map(format).mkString(", ")}" }.mkString("\n")

  private def format(x: OrderBook): String = s"""
Asks (rcv=${assetPair.priceAsset}, spt=${assetPair.amountAsset}):
${formatSide(x.asks)}

Bids (rcv=${assetPair.amountAsset}, spt=${assetPair.priceAsset}):
${formatSide(x.bids)}"""

  private def format(x: AcceptedOrder): String = {
    val name = x match {
      case _: LimitOrder  => "limit"
      case _: MarketOrder => "market"
    }

    s"""$name(a=${x.amount}, f=${x.fee}, ${format(x.order)}, rcv=${x.receiveAmount}, requiredBalance={ ${x.requiredBalance.mkString(", ")} })"""
  }

  private def format(x: Order): String = s"""Order(${x.idStr()}, a=${x.amount}, p=${x.price}, f=${x.matcherFee} ${x.feeAsset})"""
}
