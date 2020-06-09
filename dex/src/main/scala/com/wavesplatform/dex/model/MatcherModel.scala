package com.wavesplatform.dex.model

import java.math.{BigDecimal, BigInteger, RoundingMode}

import cats.instances.long.catsKernelStdGroupForLong
import cats.syntax.group._
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Price
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.error
import com.wavesplatform.dex.fp.MapImplicits.cleaningGroup
import com.wavesplatform.dex.model.AcceptedOrder.FillingInfo
import play.api.libs.json.{JsObject, JsValue, Json}

object MatcherModel {
  def getCost(amount: Long, price: Long): Long =
    new BigDecimal(price)
      .multiply(new BigDecimal(amount))
      .scaleByPowerOfTen(-Order.PriceConstantExponent)
      .setScale(0, RoundingMode.FLOOR)
      .longValue()

  def correctRateByAssetDecimals(value: Double, assetDecimals: Int): Double =
    new BigDecimal(value)
      .scaleByPowerOfTen(assetDecimals - 8)
      .doubleValue()

  sealed trait DecimalsFormat
  final case object Denormalized extends DecimalsFormat
  final case object Normalized   extends DecimalsFormat
}

case class LevelAgg(amount: Long, price: Long)

sealed trait AcceptedOrder {

  def amount: Long // could be remaining or executed, see OrderExecuted
  def fee: Long    // same
  def order: Order

  lazy val id: Order.Id = order.id()

  def price: Price = order.price
  def avgWeighedPriceNominator: BigInteger // used for avgWeighedPrice calculation, changes with order execution

  protected def rawSpentAmount: Long // Without correction

  def spentAmount: Long
  def receiveAmount: Long

  def isMarket: Boolean
  def isLimit: Boolean

  def isBuyOrder: Boolean  = order.orderType == OrderType.BUY
  def isSellOrder: Boolean = order.orderType == OrderType.SELL

  def spentAsset: Asset = order.getSpendAssetId
  def rcvAsset: Asset   = order.getReceiveAssetId
  val feeAsset: Asset   = order.feeAsset

  val matcherFee: Long = order.matcherFee

  def fillingInfo: FillingInfo = {

    val isNew           = amount == order.amount
    val filledAmount    = order.amount - amount
    val filledFee       = order.matcherFee - fee
    val avgWeighedPrice = if (isNew) 0 else avgWeighedPriceNominator.divide(BigInteger valueOf filledAmount).longValueExact()

    FillingInfo(isNew, filledAmount, filledFee, avgWeighedPrice)
  }

  def requiredFee: Long                 = fee
  def requiredBalance: Map[Asset, Long] = Map(spentAsset -> rawSpentAmount) |+| Map(feeAsset -> requiredFee)
  def reservableBalance: Map[Asset, Long]

  def availableBalanceBySpendableAssets(tradableBalance: Asset => Long): Map[Asset, Long] = {
    Set(spentAsset, feeAsset).map(asset => asset -> tradableBalance(asset)).toMap
  }

  lazy val amountOfPriceAsset: Long = new BigDecimal(amount)
    .multiply(new BigDecimal(price))
    .scaleByPowerOfTen(-Order.PriceConstantExponent)
    .setScale(0, RoundingMode.FLOOR)
    .longValue()
  lazy val amountOfAmountAsset: Long = correctedAmountOfAmountAsset(amount, price)

  protected def executionAmount(counterPrice: Price): Long = correctedAmountOfAmountAsset(amount, counterPrice)

  lazy val isValid: Boolean = isValid(price)

  def isValid(counterPrice: Price): Boolean = {
    amount > 0 && amount >= minimalAmountOfAmountAssetByPrice(counterPrice) && amount < Order.MaxAmount && spentAmount > 0 && receiveAmount > 0
  }

  protected def minimalAmountOfAmountAssetByPrice(p: Long): Long =
    Order.PriceConstantDecimal.divide(new BigDecimal(p), 0, RoundingMode.CEILING).longValue()

  protected def correctedAmountOfAmountAsset(a: Long, p: Long): Long = correctedAmountOfAmountAsset(new BigDecimal(a), new BigDecimal(p))
  protected def correctedAmountOfAmountAsset(a: BigDecimal, p: BigDecimal): Long = {
    val settledTotal = a
      .multiply(p)
      .scaleByPowerOfTen(-Order.PriceConstantExponent)
      .setScale(0, RoundingMode.FLOOR)

    settledTotal
      .scaleByPowerOfTen(Order.PriceConstantExponent)
      .divide(p, 0, RoundingMode.CEILING)
      .longValue()
  }

  def isFirstMatch: Boolean = amount == order.amount

  def forMarket(fm: MarketOrder => Unit): Unit
  def forLimit(fl: LimitOrder => Unit): Unit
}

object AcceptedOrder {

  def partialFee(matcherFee: Long, totalAmount: Long, partialAmount: Long): Long = {
    if (partialAmount > totalAmount)
      throw new IllegalArgumentException(s"partialAmount: $partialAmount should be less or equal to totalAmount: $totalAmount")
    // Should not round! It could lead to forks. See ExchangeTransactionDiff
    (BigInt(matcherFee) * partialAmount / totalAmount).toLong
  }

  /**
    * Returns executed amount obtained as a result of the match of submitted and counter orders
    *
    * For limit orders:
    *   executedAmount = matchedAmount = min(corrected amount of submitted order, corrected amount of counter order)
    *
    * For market orders:
    *
    *   Let x  = actual executed amount,
    *       a  = order amount,
    *       A  = available for spending,
    *       f  = order fee,
    *       f' = executed fee = f * (x / a),
    *       p' = counter price,
    *       c  = buy order cost (p' * x)
    *
    *   executedAmount = min(matchedAmount, x),
    *   most tricky cases occur when A < a, i.e. available for spending is not enough to execute submitted market order
    *
    *   BUY market orders:
    *
    *     1. Fee asset = spent asset
    *
    *       A = f' + c =
    *           f' + p' * x =
    *           f * (x / a) + p' * x =
    *           x * (p' + f / a)
    *
    *       x = (A * a) / (p' * a + f)
    *
    *     2. Other cases
    *
    *       A = c = p' * x
    *       x = A / p'
    *
    *   SELL market orders:
    *
    *     1. Fee asset = spent asset
    *
    *       A = f' + x =
    *           f * (x / a) + x =
    *           x (1 + f / a)
    *
    *       x = (A * a) / (a + f)
    *
    *     2. Other cases
    *
    *       x = A
    *
    */
  def executedAmount(submitted: AcceptedOrder, counter: LimitOrder): Long = {

    val matchedAmount     = math.min(submitted.executionAmount(counter.price), counter.amountOfAmountAsset)
    lazy val counterPrice = new BigDecimal(counter.price)

    def minBetweenMatchedAmountAnd(value: Long): Long            = math.min(matchedAmount, value)
    def correctByCounterPrice(value: java.math.BigDecimal): Long = submitted.correctedAmountOfAmountAsset(value, counterPrice)

    submitted match {
      case _: LimitOrder => matchedAmount
      case mo: MarketOrder =>
        minBetweenMatchedAmountAnd {
          if (mo.isBuyOrder) {
            correctByCounterPrice {
              if (mo.spentAsset == mo.feeAsset) {
                // mo.availableForSpending * mo.amount / (counter.price * mo.amount / Order.PriceConstant + mo.fee)
                val moAmount = new BigDecimal(mo.amount)
                new BigDecimal(mo.availableForSpending)
                  .multiply(moAmount)
                  .divide(
                    counterPrice
                      .multiply(moAmount)
                      .scaleByPowerOfTen(-Order.PriceConstantExponent)
                      .add(new BigDecimal(mo.fee)),
                    0,
                    RoundingMode.FLOOR
                  )
              } else {
                // mo.availableForSpending * Order.PriceConstant / counter.price
                new BigDecimal(mo.availableForSpending)
                  .scaleByPowerOfTen(Order.PriceConstantExponent)
                  .divide(counterPrice, 0, RoundingMode.FLOOR)
              }
            }
          } else if (mo.spentAsset == mo.feeAsset) {
            // mo.availableForSpending * mo.amount / (mo.amount + mo.fee)
            new BigDecimal(mo.availableForSpending)
              .multiply(new BigDecimal(mo.amount))
              .divide(new BigDecimal(mo.amount + mo.fee), 0, RoundingMode.FLOOR)
              .longValue()
          } else mo.availableForSpending
        }
    }
  }

  final case class FillingInfo(isNew: Boolean, filledAmount: Long, filledFee: Long, avgWeighedPrice: Long)
}

sealed trait BuyOrder extends AcceptedOrder {
  def receiveAmount: Long  = amountOfAmountAsset
  def spentAmount: Long    = amountOfPriceAsset
  def rawSpentAmount: Long = amountOfPriceAsset
}

sealed trait SellOrder extends AcceptedOrder {
  def receiveAmount: Long  = amountOfPriceAsset
  def spentAmount: Long    = amountOfAmountAsset
  def rawSpentAmount: Long = amount
}

sealed trait LimitOrder extends AcceptedOrder {

  def partial(amount: Long, fee: Long, avgWeighedPriceNominator: BigInteger): LimitOrder
  def reservableBalance: Map[Asset, Long] = requiredBalance

  override def isLimit: Boolean  = true
  override def isMarket: Boolean = false

  override def forMarket(fm: MarketOrder => Unit): Unit = ()
  override def forLimit(fl: LimitOrder => Unit): Unit   = fl(this)
}

object LimitOrder {
  def apply(o: Order): LimitOrder = o.orderType match {
    case OrderType.BUY  => BuyLimitOrder(o.amount, o.matcherFee, o, BigInteger.ZERO)
    case OrderType.SELL => SellLimitOrder(o.amount, o.matcherFee, o, BigInteger.ZERO)
  }
}

case class BuyLimitOrder(amount: Long, fee: Long, order: Order, avgWeighedPriceNominator: BigInteger) extends BuyOrder with LimitOrder {
  override def toString: String = s"BuyLimitOrder($amount,$fee,$id,$avgWeighedPriceNominator)"
  def partial(amount: Long, fee: Long, avgWeighedPriceNominator: BigInteger): BuyLimitOrder =
    copy(amount = amount, fee = fee, avgWeighedPriceNominator = avgWeighedPriceNominator)
}

case class SellLimitOrder(amount: Long, fee: Long, order: Order, avgWeighedPriceNominator: BigInteger) extends SellOrder with LimitOrder {
  override def toString: String = s"SellLimitOrder($amount,$fee,$id,$avgWeighedPriceNominator)"
  def partial(amount: Long, fee: Long, avgWeighedPriceNominator: BigInteger): SellLimitOrder =
    copy(amount = amount, fee = fee, avgWeighedPriceNominator = avgWeighedPriceNominator)
}

sealed trait MarketOrder extends AcceptedOrder {

  /** Min between tradable balance of the order's owner and required balance of the order by spendable asset */
  val availableForSpending: Long

  def reservableBalance: Map[Asset, Long] =
    if (availableForSpending == 0) requiredBalance - order.getSpendAssetId
    else requiredBalance.updated(order.getSpendAssetId, availableForSpending)

  def partial(amount: Long, fee: Long, availableForSpending: Long, avgWeighedPriceNominator: BigInteger): MarketOrder

  override def isLimit: Boolean  = false
  override def isMarket: Boolean = true

  override def forMarket(fm: MarketOrder => Unit): Unit = fm(this)
  override def forLimit(fl: LimitOrder => Unit): Unit   = ()
}

object MarketOrder {

  private def create(order: Order, availableForSpending: Long): MarketOrder = order.orderType match {
    case OrderType.BUY  => BuyMarketOrder(order.amount, order.matcherFee, order, availableForSpending, BigInteger.ZERO)
    case OrderType.SELL => SellMarketOrder(order.amount, order.matcherFee, order, availableForSpending, BigInteger.ZERO)
  }

  def apply(o: Order, availableForSpending: Long): MarketOrder = create(o, availableForSpending)

  def apply(o: Order, tradableBalance: Asset => Long): MarketOrder = {
    val availableForSpending = math.min(tradableBalance(o.getSpendAssetId), LimitOrder(o).requiredBalance(o.getSpendAssetId))
    create(o, availableForSpending)
  }
}

case class BuyMarketOrder(amount: Long, fee: Long, order: Order, availableForSpending: Long, avgWeighedPriceNominator: BigInteger)
    extends BuyOrder
    with MarketOrder {

  override def toString: String = s"BuyMarketOrder($amount,$fee,$id,$availableForSpending,$avgWeighedPriceNominator)"

  def partial(amount: Long, fee: Long, availableForSpending: Long, avgWeighedPriceNominator: BigInteger): BuyMarketOrder = {
    copy(amount = amount, fee = fee, availableForSpending = availableForSpending, avgWeighedPriceNominator = avgWeighedPriceNominator)
  }
}

case class SellMarketOrder(amount: Long, fee: Long, order: Order, availableForSpending: Long, avgWeighedPriceNominator: BigInteger)
    extends SellOrder
    with MarketOrder {

  override def toString: String = s"SellMarketOrder($amount,$fee,$id,$availableForSpending,$avgWeighedPriceNominator)"

  def partial(amount: Long, fee: Long, availableForSpending: Long, avgWeighedPriceNominator: BigInteger): SellMarketOrder = {
    copy(amount = amount, fee = fee, availableForSpending = availableForSpending, avgWeighedPriceNominator = avgWeighedPriceNominator)
  }
}

sealed trait OrderStatus {
  def name: String
  def json: JsValue

  def filledAmount: Long
  def filledFee: Long
}

object OrderStatus {

  sealed trait Final extends OrderStatus

  case object Accepted extends OrderStatus {

    val name           = "Accepted"
    def json: JsObject = Json.obj("status" -> name)

    override def filledAmount: Long = 0
    override def filledFee: Long    = 0
  }

  case object NotFound extends Final {

    val name           = "NotFound"
    def json: JsObject = Json.obj("status" -> name, "message" -> "The limit order is not found")

    override def filledAmount: Long = 0
    override def filledFee: Long    = 0
  }

  case class PartiallyFilled(filledAmount: Long, filledFee: Long) extends OrderStatus {
    val name: String   = PartiallyFilled.name
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }
  object PartiallyFilled {
    val name = "PartiallyFilled"
  }

  case class Filled(filledAmount: Long, filledFee: Long) extends Final {
    val name: String   = Filled.name
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }
  object Filled {
    val name = "Filled"
  }

  case class Cancelled(filledAmount: Long, filledFee: Long) extends Final {
    val name: String   = Cancelled.name
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }
  object Cancelled {
    val name = "Cancelled"
  }

  def finalCancelStatus(ao: AcceptedOrder, isSystemCancel: Boolean): Final = {
    val filledAmount     = ao.order.amount - ao.amount
    val filledMatcherFee = ao.order.matcherFee - ao.fee
    if (isSystemCancel && (filledAmount > 0 || ao.isMarket)) Filled(filledAmount, filledMatcherFee) else Cancelled(filledAmount, filledMatcherFee)
  }
}

object Events {

  sealed trait Event extends Product with Serializable {
    def timestamp: Long
  }

  /**
    * In case of dynamic fee settings the following params can be different from the appropriate `acceptedOrder.order.matcherFee`
    */
  case class OrderExecuted(submitted: AcceptedOrder, counter: LimitOrder, timestamp: Long, counterExecutedFee: Price, submittedExecutedFee: Price)
      extends Event {

    def executedPrice: Long                   = counter.price
    lazy val executedAmount: Long             = AcceptedOrder.executedAmount(submitted, counter)
    lazy val executedAmountOfPriceAsset: Long = MatcherModel.getCost(executedAmount, executedPrice)

    def counterRemainingAmount: Long      = math.max(counter.amount - executedAmount, 0)
    def counterRemainingFee: Long         = math.max(counter.fee - counterExecutedFee, 0)
    lazy val counterRemaining: LimitOrder = counter.partial(counterRemainingAmount, counterRemainingFee, executedWeighedPriceNominator)

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0)
    def submittedRemainingFee: Long    = math.max(submitted.fee - submittedExecutedFee, 0)

    def executedWeighedPriceNominator: BigInteger = (BigInt(executedAmount) * counter.price).bigInteger

    def submittedMarketRemaining(submittedMarketOrder: MarketOrder): MarketOrder = {

      val spentAmount = if (submitted.isSellOrder) executedAmount else executedAmountOfPriceAsset
      val spentFee    = if (submitted.feeAsset == submitted.spentAsset) submittedExecutedFee else 0L

      submittedMarketOrder.partial(
        amount = submittedRemainingAmount,
        fee = submittedRemainingFee,
        availableForSpending = submittedMarketOrder.availableForSpending - spentAmount - spentFee,
        avgWeighedPriceNominator = submittedMarketOrder.avgWeighedPriceNominator.add(executedWeighedPriceNominator)
      )
    }

    def submittedLimitRemaining(submittedLimitOrder: LimitOrder): LimitOrder = {
      submittedLimitOrder.partial(
        amount = submittedRemainingAmount,
        fee = submittedRemainingFee,
        avgWeighedPriceNominator = submittedLimitOrder.avgWeighedPriceNominator.add(executedWeighedPriceNominator)
      )
    }

    lazy val submittedRemaining: AcceptedOrder = submitted match {
      case lo: LimitOrder  => submittedLimitRemaining(lo)
      case mo: MarketOrder => submittedMarketRemaining(mo)
    }
  }

  case class OrderAdded(order: AcceptedOrder, timestamp: Long) extends Event

  case class OrderCanceled(acceptedOrder: AcceptedOrder, isSystemCancel: Boolean, timestamp: Long) extends Event

  case class OrderCancelFailed(id: Order.Id, reason: error.MatcherError)

  case class ExchangeTransactionCreated(tx: ExchangeTransaction, reason: OrderExecuted)
}
