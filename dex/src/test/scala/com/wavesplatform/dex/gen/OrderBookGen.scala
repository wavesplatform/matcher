package com.wavesplatform.dex.gen

import java.nio.charset.StandardCharsets

import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{AcceptedOrder, BuyLimitOrder, BuyMarketOrder, LimitOrder, MarketOrder, OrderBook, OrderBookSnapshot, SellLimitOrder, SellMarketOrder}
import org.scalacheck.Gen

import scala.collection.JavaConverters._

trait OrderBookGen {
  protected val matcher = KeyPair(ByteStr("matcher".getBytes(StandardCharsets.UTF_8)))

  protected val maxLevelsInOrderBook = 6
  protected val maxOrdersInLevel     = 2

  protected val ts         = 1000L
  protected val expiration = ts + 60000L

  protected val clients   = (1 to 3).map(clientId => KeyPair(ByteStr(s"client-$clientId".getBytes(StandardCharsets.UTF_8))))
  protected val clientGen = Gen.oneOf(clients)

  protected val assetPair = AssetPair(
    amountAsset = IssuedAsset(ByteStr("amount-asset".getBytes(StandardCharsets.UTF_8))),
    priceAsset = Waves
  )
  protected val thirdAsset = IssuedAsset(ByteStr("third-asset".getBytes(StandardCharsets.UTF_8)))
  protected val assetGen   = Gen.oneOf[Asset](assetPair.amountAsset, assetPair.priceAsset, thirdAsset)

  protected val orderSideGen: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  protected def mkOrderBook(askOrders: Seq[LimitOrder], bidOrders: Seq[LimitOrder]): OrderBook =
    OrderBook(
      OrderBookSnapshot(
        asks = askOrders.groupBy(_.order.price),
        bids = bidOrders.groupBy(_.order.price),
        lastTrade = None
      ))

  protected def sideOrdersGen(side: OrderType, maxLevels: Int, pricesGen: Gen[Long]): Gen[(Int, Seq[LimitOrder])] =
    for {
      levels <- Gen.choose(0, maxLevels)
      prices <- Gen.listOfN(levels, pricesGen)
      orders <- Gen.sequence(prices.map { price =>
        Gen.resize(maxOrdersInLevel, Gen.listOf(limitOrderGen(orderGen(Gen.const(price), side))))
      })
    } yield (levels, orders.asScala.flatten)

  protected def limitOrderGen(orderGen: Gen[Order]): Gen[LimitOrder] =
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

  protected def marketOrderGen(orderGen: Gen[Order]): Gen[MarketOrder] =
    for {
      order                <- orderGen
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
  protected def orderGen(pricesGen: Gen[Long], side: OrderType): Gen[Order] =
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

  protected def minAmount(price: Long): Long = math.max(1, Order.PriceConstant / price)
}
