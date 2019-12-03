package com.wavesplatform.dex.model

import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.MatcherError
import com.wavesplatform.dex.model.MatcherModel.Price
import com.wavesplatform.state.{Blockchain, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange._
import play.api.libs.json.{JsObject, JsValue, Json}

import scala.math.BigDecimal.RoundingMode

object MatcherModel {

  type Price  = Long
  type Amount = Long

  def getAssetDecimals(asset: Asset, blockchain: Blockchain): Either[MatcherError, Int] =
    asset.fold[Either[MatcherError, Int]](Right(8)) { issuedAsset =>
      blockchain
        .assetDescription(issuedAsset)
        .toRight[MatcherError] { error.AssetNotFound(issuedAsset) }
        .map(_.decimals)
    }

  def getPairDecimals(pair: AssetPair, blockchain: Blockchain): Either[MatcherError, (Int, Int)] =
    (getAssetDecimals(pair.amountAsset, blockchain), getAssetDecimals(pair.priceAsset, blockchain)).tupled

  def getCost(amount: Long, price: Long): Long = (BigDecimal(price) * amount / Order.PriceConstant).toLong

  /** Converts amounts, prices and fees from denormalized values (decimal numbers) to normalized ones (longs) */
  object Normalization {

    def normalizeAmountAndFee(value: Double, assetDecimals: Int): Amount =
      (BigDecimal(value) * BigDecimal(10).pow(assetDecimals)).toLong

    def normalizePrice(value: Double, amountAssetDecimals: Int, priceAssetDecimals: Int): Price =
      (BigDecimal(value) * BigDecimal(10).pow(8 + priceAssetDecimals - amountAssetDecimals).toLongExact).toLong

    def normalizePrice(value: Double, pair: AssetPair, decimals: (Int, Int)): Price = {
      val (amountAssetDecimals, priceAssetDecimals) = decimals
      normalizePrice(value, amountAssetDecimals, priceAssetDecimals)
    }
  }

  /** Converts amounts, prices and fees from normalized values (longs) to denormalized ones (decimal numbers) */
  object Denormalization {

    def denormalizeAmountAndFee(value: Amount, assetDecimals: Int): Double =
      (BigDecimal(value) / BigDecimal(10).pow(assetDecimals)).toDouble

    def denormalizeAmountAndFee(value: Amount, asset: Asset, blockchain: Blockchain): Either[MatcherError, Double] =
      getAssetDecimals(asset, blockchain).map(denormalizeAmountAndFee(value, _))

    def denormalizeAmountAndFeeWithDefault(value: Amount, asset: Asset, blockchain: Blockchain): Double =
      denormalizeAmountAndFee(value, asset, blockchain).getOrElse { (value / BigDecimal(10).pow(8)).toDouble }

    def denormalizePrice(value: Price, amountAssetDecimals: Int, priceAssetDecimals: Int): Double =
      (BigDecimal(value) / BigDecimal(10).pow(8 + priceAssetDecimals - amountAssetDecimals).toLongExact).toDouble

    def denormalizePrice(value: Price, pair: AssetPair, decimals: (Int, Int)): Double = {
      val (amountAssetDecimals, priceAssetDecimals) = decimals
      denormalizePrice(value, amountAssetDecimals, priceAssetDecimals)
    }

    def denormalizePrice(value: Price, pair: AssetPair, blockchain: Blockchain): Either[MatcherError, Double] =
      getPairDecimals(pair, blockchain).map(denormalizePrice(value, pair, _))

    def denormalizePriceWithDefault(value: Price, pair: AssetPair, blockchain: Blockchain): Double =
      denormalizePrice(value, pair, blockchain).getOrElse { (value / BigDecimal(10).pow(8)).toDouble }
  }

  def correctRateByAssetDecimals(value: Double, assetDecimals: Int): Double = { BigDecimal(value) * BigDecimal(10).pow(assetDecimals - 8) }.toDouble

  sealed trait DecimalsFormat
  final case object Denormalized extends DecimalsFormat
  final case object Normalized   extends DecimalsFormat
}

case class LevelAgg(amount: Long, price: Long)

sealed trait AcceptedOrder {

  def amount: Long // could be remaining or executed, see OrderExecuted
  def fee: Long    // same
  def order: Order

  def price: Price = order.price

  protected def rawSpentAmount: Long // Without correction

  def spentAmount: Long
  def receiveAmount: Long

  def isMarket: Boolean = this.fold(_ => false)(_ => true)
  def isLimit: Boolean  = this.fold(_ => true)(_ => false)

  def isBuyOrder: Boolean  = order.orderType == OrderType.BUY
  def isSellOrder: Boolean = order.orderType == OrderType.SELL

  def spentAsset: Asset = order.getSpendAssetId
  def rcvAsset: Asset   = order.getReceiveAssetId
  val feeAsset: Asset   = order.matcherFeeAssetId

  def requiredFee: Price                = if (feeAsset == rcvAsset) (fee - receiveAmount).max(0L) else fee
  def requiredBalance: Map[Asset, Long] = Map(spentAsset -> rawSpentAmount) |+| Map(feeAsset -> requiredFee)
  def reservableBalance: Map[Asset, Long]

  def availableBalanceBySpendableAssets(tradableBalance: Asset => Long): Map[Asset, Long] = {
    Set(spentAsset, feeAsset).map(asset => asset -> tradableBalance(asset)).toMap
  }

  def amountOfPriceAsset: Long  = (BigDecimal(amount) * price / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
  def amountOfAmountAsset: Long = correctedAmountOfAmountAsset(amount, price)

  protected def executionAmount(counterPrice: Price): Long = correctedAmountOfAmountAsset(amount, counterPrice)

  def isValid: Boolean = isValid(price)
  def isValid(counterPrice: Price): Boolean =
    amount > 0 && amount >= minimalAmountOfAmountAssetByPrice(counterPrice) && amount < Order.MaxAmount && spentAmount > 0 && receiveAmount > 0

  private def minimalAmountOfAmountAssetByPrice(p: Long): Long = (BigDecimal(Order.PriceConstant) / p).setScale(0, RoundingMode.CEILING).toLong

  protected def correctedAmountOfAmountAsset(a: Long, p: Long): Long = {
    val settledTotal = (BigDecimal(p) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / p * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def fold[A](fl: LimitOrder => A)(fm: MarketOrder => A): A = this match {
    case lo: LimitOrder  => fl(lo)
    case mo: MarketOrder => fm(mo)
  }
}

object AcceptedOrder {

  def partialFee(matcherFee: Long, totalAmount: Long, partialAmount: Long): Long = {
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
    *       A = x
    *
    */
  def executedAmount(submitted: AcceptedOrder, counter: LimitOrder): Long = {

    val matchedAmount                                 = math.min(submitted.executionAmount(counter.price), counter.amountOfAmountAsset)
    def minBetweenMatchedAmountAnd(value: Long): Long = math.min(matchedAmount, value)
    def correctByCounterPrice(value: Long): Long      = submitted.correctedAmountOfAmountAsset(value, counter.price)

    submitted.fold(_ => matchedAmount) {
      case mo if mo.isBuyOrder =>
        if (mo.spentAsset == mo.feeAsset)
          minBetweenMatchedAmountAnd {
            correctByCounterPrice {
              (BigDecimal(mo.availableForSpending) * mo.amount / (BigDecimal(counter.price) * mo.amount / Order.PriceConstant + mo.fee)).toLong
            }
          } else
          minBetweenMatchedAmountAnd {
            correctByCounterPrice {
              (BigDecimal(mo.availableForSpending) * Order.PriceConstant / counter.price).toLong
            }
          }
      case mo =>
        if (mo.spentAsset == mo.feeAsset)
          minBetweenMatchedAmountAnd { (BigDecimal(mo.availableForSpending) * mo.amount / (mo.amount + mo.fee)).toLong } else
          minBetweenMatchedAmountAnd { mo.availableForSpending }
    }
  }
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
  def partial(amount: Long, fee: Long): LimitOrder
  def reservableBalance: Map[Asset, Long] = requiredBalance
}

object LimitOrder {

  def apply(o: Order): LimitOrder = {

    val pf = AcceptedOrder.partialFee(o.matcherFee, o.amount, o.amount)

    o.orderType match {
      case OrderType.BUY  => BuyLimitOrder(o.amount, pf, o)
      case OrderType.SELL => SellLimitOrder(o.amount, pf, o)
    }
  }
}

case class BuyLimitOrder(amount: Long, fee: Long, order: Order) extends BuyOrder with LimitOrder {
  override def toString: String                       = s"BuyLimitOrder($amount,$fee,${order.id()})"
  def partial(amount: Long, fee: Long): BuyLimitOrder = copy(amount = amount, fee = fee)
}

case class SellLimitOrder(amount: Long, fee: Long, order: Order) extends SellOrder with LimitOrder {
  override def toString: String                        = s"SellLimitOrder($amount,$fee,${order.id()})"
  def partial(amount: Long, fee: Long): SellLimitOrder = copy(amount = amount, fee = fee)
}

sealed trait MarketOrder extends AcceptedOrder {
  val availableForSpending: Long
  def reservableBalance: Map[Asset, Long] = requiredBalance.updated(order.getSpendAssetId, availableForSpending)
  def partial(amount: Long, fee: Long, availableForSpending: Long): MarketOrder
}

object MarketOrder {

  private def create(order: Order, availableForSpending: Long): MarketOrder = {
    val pf = AcceptedOrder.partialFee(order.matcherFee, order.amount, order.amount)
    order.orderType match {
      case OrderType.BUY  => BuyMarketOrder(order.amount, pf, order, availableForSpending)
      case OrderType.SELL => SellMarketOrder(order.amount, pf, order, availableForSpending)
    }
  }

  def apply(o: Order, availableForSpending: Long): MarketOrder = create(o, availableForSpending)

  def apply(o: Order, tradableBalance: Asset => Long): MarketOrder = {
    val availableForSpending = math.min(tradableBalance(o.getSpendAssetId), LimitOrder(o).requiredBalance(o.getSpendAssetId))
    create(o, availableForSpending)
  }
}

case class BuyMarketOrder(amount: Long, fee: Long, order: Order, availableForSpending: Long) extends BuyOrder with MarketOrder {

  override def toString: String = s"BuyMarketOrder($amount,$fee,${order.id()},$availableForSpending)"

  def partial(amount: Long, fee: Long, availableForSpending: Long): BuyMarketOrder = {
    copy(amount = amount, fee = fee, availableForSpending = availableForSpending)
  }
}

case class SellMarketOrder(amount: Long, fee: Long, order: Order, availableForSpending: Long) extends SellOrder with MarketOrder {

  override def toString: String = s"SellMarketOrder($amount,$fee,${order.id()},$availableForSpending)"

  def partial(amount: Long, fee: Long, availableForSpendings: Long): SellMarketOrder = {
    copy(amount = amount, fee = fee, availableForSpending = availableForSpendings)
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
    val name           = "PartiallyFilled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }

  case class Filled(filledAmount: Long, filledFee: Long) extends Final {
    val name           = "Filled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }

  case class Cancelled(filledAmount: Long, filledFee: Long) extends Final {
    val name           = "Cancelled"
    def json: JsObject = Json.obj("status" -> name, "filledAmount" -> filledAmount, "filledFee" -> filledFee)
  }

  def finalStatus(ao: AcceptedOrder, isSystemCancel: Boolean): Final = {
    val filledAmount     = ao.order.amount - ao.amount
    val filledMatcherFee = ao.order.matcherFee - ao.fee
    if (isSystemCancel && (filledAmount > 0 || ao.isMarket)) Filled(filledAmount, filledMatcherFee) else Cancelled(filledAmount, filledMatcherFee)
  }
}

object Events {

  sealed trait Event

  case class OrderExecuted(submitted: AcceptedOrder, counter: LimitOrder, timestamp: Long) extends Event {

    lazy val executedAmount: Long             = AcceptedOrder.executedAmount(submitted, counter)
    lazy val executedAmountOfPriceAsset: Long = MatcherModel.getCost(executedAmount, counter.price)

    def counterRemainingAmount: Long = math.max(counter.amount - executedAmount, 0)
    def counterExecutedFee: Long     = AcceptedOrder.partialFee(counter.order.matcherFee, counter.order.amount, executedAmount)
    def counterRemainingFee: Long    = math.max(counter.fee - counterExecutedFee, 0)
    def counterRemaining: LimitOrder = counter.partial(amount = counterRemainingAmount, fee = counterRemainingFee)

    def submittedRemainingAmount: Long = math.max(submitted.amount - executedAmount, 0)
    def submittedExecutedFee: Long     = AcceptedOrder.partialFee(submitted.order.matcherFee, submitted.order.amount, executedAmount)
    def submittedRemainingFee: Long    = math.max(submitted.fee - submittedExecutedFee, 0)

    def submittedMarketRemaining(submittedMarketOrder: MarketOrder): MarketOrder = {

      val spentAmount = if (submitted.isSellOrder) executedAmount else executedAmountOfPriceAsset
      val spentFee    = if (submitted.feeAsset == submitted.spentAsset) submittedExecutedFee else 0L

      submittedMarketOrder.partial(
        amount = submittedRemainingAmount,
        fee = submittedRemainingFee,
        availableForSpending = submittedMarketOrder.availableForSpending - spentAmount - spentFee
      )
    }

    def submittedLimitRemaining(submittedLimitOrder: LimitOrder): LimitOrder = {
      submittedLimitOrder.partial(amount = submittedRemainingAmount, fee = submittedRemainingFee)
    }

    def submittedRemaining: AcceptedOrder = submitted.fold[AcceptedOrder] { submittedLimitRemaining } { submittedMarketRemaining }
  }

  case class OrderAdded(order: LimitOrder, timestamp: Long) extends Event

  case class OrderCanceled(acceptedOrder: AcceptedOrder, isSystemCancel: Boolean, timestamp: Long) extends Event

  case class OrderCancelFailed(id: Order.Id, reason: error.MatcherError)

  case class ExchangeTransactionCreated(tx: ExchangeTransaction)

  case class BalanceChanged(changes: Map[Address, BalanceChanged.Changes]) {
    def isEmpty: Boolean = changes.isEmpty
  }

  object BalanceChanged {
    val empty: BalanceChanged = BalanceChanged(Map.empty)
    case class Changes(updatedPortfolio: Portfolio, changedAssets: Set[Option[Asset]])
  }
}
