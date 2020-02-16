package com.wavesplatform.dex.model

import java.nio.charset.StandardCharsets

import cats.instances.long.catsKernelStdGroupForLong
import cats.kernel.Monoid
import cats.syntax.group._
import com.wavesplatform.dex.domain.account.{KeyPair, PublicKey}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class OrderBookSpec extends AnyFreeSpecLike with MatcherSpecBase with Matchers with ScalaCheckPropertyChecks with NoShrink {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 1000)

  private val matcher = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8)))
  private val assetPair = AssetPair(
    amountAsset = IssuedAsset(ByteStr("amount-asset".getBytes(StandardCharsets.UTF_8))),
    priceAsset = Waves
  )
  private val thirdAsset = IssuedAsset(ByteStr("third-asset".getBytes(StandardCharsets.UTF_8)))
  private val ts         = 1000L
  private val expiration = ts + 60000L

  private val assetGen = Gen.oneOf[Asset](assetPair.amountAsset, assetPair.priceAsset, thirdAsset)

  private val clients   = (1 to 3).map(clientId => KeyPair(ByteStr(s"client-$clientId".getBytes(StandardCharsets.UTF_8))))
  private val clientGen = Gen.oneOf(clients)

  private val maxOrdersInOrderBook = 6

  // TODO migrate to long ranges in 2.13
  private val askPricesMin = 1000L * Order.PriceConstant
  private val askPricesMax = 2000L * Order.PriceConstant
  private val askPricesGen = Gen.choose(askPricesMin, askPricesMax)

  private val bidPricesMin = 1L * Order.PriceConstant
  private val bidPricesMax = 999L * Order.PriceConstant
  private val bidPricesGen = Gen.choose(bidPricesMin, bidPricesMax)

  private val allPricesGen = Gen.choose(bidPricesMin, askPricesMax)

  private val askLimitOrderGen: Gen[LimitOrder] = limitOrderGen(orderGen(askPricesGen, OrderType.SELL))
  private val bidLimitOrderGen: Gen[LimitOrder] = limitOrderGen(orderGen(bidPricesGen, OrderType.BUY))
  private val newOrderGen: Gen[AcceptedOrder] = orderTypeGenerator.flatMap { x =>
    Gen.oneOf(
      limitOrderGen(orderGen(allPricesGen, x)),
      marketOrderGen(orderGen(allPricesGen, x))
    )
  }

  private val invariantTestGen = for {
    asksNumber <- Gen.choose(1, maxOrdersInOrderBook)
    askOrders  <- Gen.listOfN(asksNumber, askLimitOrderGen)
    bidOrders  <- Gen.listOfN(maxOrdersInOrderBook - asksNumber, bidLimitOrderGen)
    newOrder   <- newOrderGen
  } yield (askOrders, bidOrders, newOrder)

  "coins invariant" in forAll(invariantTestGen) {
    case (askOrders, bidOrders, newOrder) =>
      val ob             = mkOrderBook(askOrders, bidOrders)
      val obBefore       = format(ob)
      val balancesBefore = balancesBy(ob) |+| balancesBy(newOrder)
      val coinsBefore    = Monoid.combineAll(balancesBefore.values)

      val events = ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee))

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

          submittedSpent should matchTo(counterReceive)
          counterSpent should matchTo(submittedReceive)

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
        s"""
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

  private def limitOrderGen(orderGen: Gen[Order]): Gen[LimitOrder] =
    for {
      order      <- orderGen
      restAmount <- Gen.choose(minAmount(order.price), order.amount)
    } yield {
      val restFee = AcceptedOrder.partialFee(order.matcherFee, order.amount, restAmount)
      order.orderType match {
        case OrderType.SELL => SellLimitOrder(restAmount, restFee, order)
        case OrderType.BUY  => BuyLimitOrder(restAmount, restFee, order)
      }
    }

  private def marketOrderGen(orderGen: Gen[Order]): Gen[MarketOrder] =
    for {
      order <- orderGen
      availableForSpending <- Gen.choose(minAmount(order.price), order.amount)
    } yield {
      order.orderType match {
        case OrderType.SELL => SellMarketOrder(order.amount, order.matcherFee, order, availableForSpending)
        case OrderType.BUY  => BuyMarketOrder(order.amount, order.matcherFee, order, availableForSpending)
      }
    }

  /**
    * @param pricesGen Should be multiplied by Order.PriceConstant
    */
  private def orderGen(pricesGen: Gen[Long], side: OrderType): Gen[Order] =
    for {
      owner    <- clientGen
      feeAsset <- assetGen
      price    <- pricesGen
      amount <- {
        // The rule based on getReceiveAmount (for SELL orders) or getSpendAmount (for BUY orders)
        // In both cases we get same condition (20 here to escape cases when sum > Long.MaxValue):
        // amount: 1 <= amount * price / PriceConstant <= Long.MaxValue / 20
        val maxValue = BigInt(Long.MaxValue / 20) * Order.PriceConstant / price
        Gen.chooseNum(minAmount(price), maxValue.min(Long.MaxValue / 20).toLong)
      }
      matcherFee <- Gen.choose(0, 300000)
      version    <- if (feeAsset == Waves) Gen.choose[Byte](1, 3) else Gen.const(3: Byte)
    } yield
      Order(
        sender = owner,
        matcher = matcher,
        pair = assetPair,
        orderType = side,
        amount = amount,
        price = price,
        timestamp = ts,
        expiration = expiration,
        matcherFee = matcherFee,
        version = version,
        feeAsset = feeAsset
      )

  private def balancesBy(ob: OrderBook): Map[PublicKey, Map[Asset, Long]] =
    ob.allOrders.foldLeft(Map.empty[PublicKey, Map[Asset, Long]]) {
      case (r, (_, o)) => r |+| balancesBy(o)
    }

  private def balancesBy(o: AcceptedOrder): Map[PublicKey, Map[Asset, Long]] = Map(o.order.senderPublicKey -> o.requiredBalance)

  private def minAmount(price: Long): Long = math.max(1, Order.PriceConstant / price)

  private def mkOrderBook(askOrders: List[LimitOrder], bidOrders: List[LimitOrder]): OrderBook =
    OrderBook(
      OrderBookSnapshot(
        asks = askOrders.groupBy(_.order.price),
        bids = bidOrders.groupBy(_.order.price),
        lastTrade = None
      ))

  private def formatSide(xs: Iterable[(Long, Level)]): String =
    xs.map { case (p, orders) => s"$p -> ${orders.map(format).mkString(", ")}" }.mkString("\n")

  private def format(x: OrderBook): String = s"""
Asks (rcv=${assetPair.priceAsset}, spt=${assetPair.amountAsset}):
${formatSide(x.getAsks)}

Bids (rcv=${assetPair.amountAsset}, spt=${assetPair.priceAsset}):
${formatSide(x.getBids)}"""

  private def format(x: AcceptedOrder): String = {
    val name = x match {
      case _: LimitOrder  => "limit"
      case _: MarketOrder => "market"
    }

    s"""$name(a=${x.amount}, f=${x.fee}, ${format(x.order)}, rcv=${x.receiveAmount}, requiredBalance={ ${x.requiredBalance.mkString(", ")} })"""
  }

  private def format(x: Order): String = s"""Order(${x.idStr()}, a=${x.amount}, p=${x.price}, f=${x.matcherFee} ${x.feeAsset})"""
}
