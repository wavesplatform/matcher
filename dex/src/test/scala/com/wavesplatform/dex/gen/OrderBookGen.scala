package com.wavesplatform.dex.gen

import java.math.BigInteger
import java.nio.charset.StandardCharsets

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{AcceptedOrder, BuyLimitOrder, BuyMarketOrder, LimitOrder, MarketOrder, OrderBook, OrderBookSnapshot, SellLimitOrder, SellMarketOrder}
import org.scalacheck.Gen

trait OrderBookGen {

  val matcher: KeyPair = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8)))

  val ts: Long = 1000L
  val expiration: Long = ts + 60000L

  val clients = (1 to 3).map(clientId => KeyPair(ByteStr(s"client-$clientId".getBytes(StandardCharsets.UTF_8))))
  val clientGen = Gen.oneOf(clients)

  val assetPair = AssetPair(
    amountAsset = IssuedAsset(ByteStr("amount-asset".getBytes(StandardCharsets.UTF_8))),
    priceAsset = Waves
  )

  val thirdAsset = IssuedAsset(ByteStr("third-asset".getBytes(StandardCharsets.UTF_8)))
  val assetGen = Gen.oneOf[Asset](assetPair.amountAsset, assetPair.priceAsset, thirdAsset)

  val orderSideGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  def mkOrderBook(askOrders: Seq[LimitOrder], bidOrders: Seq[LimitOrder]): OrderBook =
    OrderBook(
      OrderBookSnapshot(
        asks = askOrders.groupBy(_.order.price),
        bids = bidOrders.groupBy(_.order.price),
        lastTrade = None,
        nextTxTimestamp = 0L
      )
    )

  def flexibleSidesOrdersGen(
    maxLevelsInOrderBook: Int,
    maxOrdersInLevel: Int,
    askPricesGen: Gen[Long],
    bidPricesGen: Gen[Long]
  ): Gen[(Seq[LimitOrder], Seq[LimitOrder])] =
    for {
      (asksLevels, askOrders) <- flexibleSideOrdersGen(OrderType.SELL, maxLevelsInOrderBook, maxOrdersInLevel, askPricesGen)
      (_, bidOrders) <- flexibleSideOrdersGen(OrderType.BUY, maxLevelsInOrderBook - asksLevels, maxOrdersInLevel, bidPricesGen)
    } yield (askOrders, bidOrders)

  def flexibleSideOrdersGen(side: OrderType, maxLevels: Int, maxOrdersInLevel: Int, pricesGen: Gen[Long]): Gen[(Int, Seq[LimitOrder])] =
    for {
      levelNumber <- Gen.choose(0, maxLevels)
      prices <- Gen.listOfN(levelNumber, pricesGen)
      orders <- Gen.sequence[List[List[LimitOrder]], List[LimitOrder]](
        prices.map { price =>
          Gen.chooseNum(1, maxOrdersInLevel).flatMap { size =>
            Gen.listOfN(size, limitOrderGen(orderGen(Gen.const(price), side)))
          }
        }
      )
    } yield (levelNumber, orders.flatten)

  def limitOrderGen(orderGen: Gen[Order]): Gen[LimitOrder] =
    for {
      order <- orderGen
      restAmount <- Gen.choose(minAmount(order.price), order.amount)
    } yield {
      val restFee = AcceptedOrder.partialFee(order.matcherFee, order.amount, restAmount)
      order.orderType match {
        case OrderType.SELL => SellLimitOrder(restAmount, restFee, order, BigInteger.valueOf(order.price))
        case OrderType.BUY => BuyLimitOrder(restAmount, restFee, order, BigInteger.valueOf(order.price))
      }
    }

  def marketOrderGen(orderGen: Gen[Order]): Gen[MarketOrder] =
    for {
      order <- orderGen
      availableForSpending <- Gen.choose(minAmount(order.price), order.amount)
    } yield order.orderType match {
      case OrderType.SELL => SellMarketOrder(order.amount, order.matcherFee, order, availableForSpending, BigInteger.valueOf(order.price))
      case OrderType.BUY => BuyMarketOrder(order.amount, order.matcherFee, order, availableForSpending, BigInteger.valueOf(order.price))
    }

  /**
   * @param pricesGen Should be multiplied by Order.PriceConstant
   */
  def orderGen(pricesGen: Gen[Long], side: OrderType): Gen[Order] =
    for {
      owner <- clientGen
      order <- orderGen(owner, pricesGen, side)
    } yield order

  /**
   * @param pricesGen Should be multiplied by Order.PriceConstant
   */
  def orderGen(owner: KeyPair, pricesGen: Gen[Long], side: OrderType): Gen[Order] =
    for {
      feeAsset <- assetGen
      price <- pricesGen
      amount <- {
        // The rule based on getReceiveAmount (for SELL orders) or getSpendAmount (for BUY orders)
        // In both cases we get same condition (20 here to escape cases when sum > Long.MaxValue):
        // amount: 1 <= amount * price / PriceConstant <= Long.MaxValue / 20
        val maxValue = BigInt(Long.MaxValue / 20) * Order.PriceConstant / price
        Gen.chooseNum(minAmount(price), maxValue.min(Long.MaxValue / 20).toLong)
      }
      matcherFee <- Gen.choose(0, 300000)
      version <- if (feeAsset == Waves) Gen.choose[Byte](1, 3) else Gen.const(3: Byte)
    } yield Order(
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

  def minAmount(price: Long): Long = math.max(1, Order.PriceConstant / price)
}
