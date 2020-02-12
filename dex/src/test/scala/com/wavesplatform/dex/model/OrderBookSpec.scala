package com.wavesplatform.dex.model

import java.nio.charset.StandardCharsets

import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group._
import cats.kernel.{Group, Monoid}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.fp.MapImplicits.group
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalacheck.Gen.Choose
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.NumericRange

class OrderBookSpec extends AnyFreeSpecLike with MatcherSpecBase with Matchers with ScalaCheckPropertyChecks with NoShrink {

  private val matcher = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8)))
  private val assetPair = AssetPair(
    amountAsset = IssuedAsset(ByteStr("amount-asset".getBytes(StandardCharsets.UTF_8))),
    priceAsset = Waves
  )
  private val thirdAsset = IssuedAsset(ByteStr("third-asset".getBytes(StandardCharsets.UTF_8)))
  private val ts         = 1000L
  private val expiration = ts + 60000L

  private val assetGen = Gen.oneOf[Asset](assetPair.amountAsset, assetPair.priceAsset, thirdAsset)

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
  private val limitOrderGen: Gen[LimitOrder] = orderTypeGenerator.flatMap { x =>
    limitOrderGen(orderGen(allPricesGen, x))
  }

  "coins invariant" in forAll(
    Gen.listOfN(3, askLimitOrderGen),
    Gen.listOfN(3, bidLimitOrderGen),
    limitOrderGen
  ) { (askOrders, bidOrders, newOrder) =>
    val bids = bidOrders.groupBy(_.order.price)
    val asks = askOrders.groupBy(_.order.price)

    val ob = OrderBook(OrderBookSnapshot(bids, asks, lastTrade = None))

//    val coinsBefore = Monoid.combineAll((newOrder :: askOrders ::: bidOrders).map(_.requiredBalance))
    val coinsBefore = Monoid.combine(newOrder.requiredBalance, countCoins(ob))
    val events      = ob.add(newOrder, ts, getMakerTakerFee = (o1, o2) => (o1.matcherFee, o2.matcherFee))
    val coinsAfter  = countCoins(ob) //Monoid.combine()
    events.map {
      case evt: Events.OrderExecuted =>
        Monoid.combine(
          Group.inverse(Monoid.combine(evt.submitted.requiredBalance, evt.counter.requiredBalance)),
          Map(
            evt.submitted.rcvAsset -> evt.submitted.receiveAmount,
            evt.counter.rcvAsset   -> evt.counter.receiveAmount
          )
        )

      case evt: Events.OrderCanceled => evt.acceptedOrder.requiredBalance
      case _: Events.OrderAdded      => Map.empty
    }

    val diff = coinsBefore |-| coinsAfter
    val clue =
      s"""
Asks: 
${asks.mkString("\n")}
Bids:
${bids.mkString("\n")}
Order:
$newOrder
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

  /**
    * @param pricesGen Should be multiplied by Order.PriceConstant
    */
  private def orderGen(pricesGen: Gen[Long], side: OrderType): Gen[Order] =
    for {
      owner    <- accountGen
      feeAsset <- assetGen
      price    <- pricesGen
      amount <- {
        // The rule based on getReceiveAmount (for SELL orders) or getSpendAmount (for BUY orders)
        // In both cases we get same condition:
        // amount: 1 <= amount * price / PriceConstant <= Long.MaxValue
        val maxValue = (BigInt(Long.MaxValue) * Order.PriceConstant / price)
        Gen.chooseNum(
          minAmount(price),
          if (maxValue.isValidLong) maxValue.toLong else Long.MaxValue
        )
      }
      version <- if (feeAsset == Waves) Gen.choose[Byte](1, 3) else Gen.const(3: Byte)
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

  private def choose[T: Numeric: Choose](range: NumericRange.Inclusive[T]): Gen[T] = Gen.chooseNum(range.head, range.last)

  private def countCoins(ob: OrderBook): Map[Asset, Long] = Monoid.combineAll((ob.getAsks.values ++ ob.getBids.values).flatten.map(_.requiredBalance))
  private def minAmount(price: Long): Long                = math.max(1, Order.PriceConstant / price)
}
