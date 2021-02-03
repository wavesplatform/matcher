package com.wavesplatform.dex.api.ws.entities

import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.doubleAsStringFormat
import com.wavesplatform.dex.api.ws.entities.WsFullOrder.WsExecutionInfo
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsFullOrder(
  id: Order.Id,
  owner: Address,
  timestamp: Long,
  amountAsset: Asset,
  priceAsset: Asset,
  side: OrderType,
  isMarket: Boolean,
  price: Double,
  amount: Double,
  fee: Double,
  feeAsset: Asset,
  status: String,
  filledAmount: Double,
  filledFee: Double,
  avgWeighedPrice: Double,
  eventTimestamp: Long,
  executionInfo: Option[WsExecutionInfo]
)

object WsFullOrder {

  def apply(
    id: Order.Id,
    owner: Address,
    timestamp: Long,
    amountAsset: Asset,
    priceAsset: Asset,
    side: OrderType,
    isMarket: Boolean,
    price: Double,
    amount: Double,
    fee: Double,
    feeAsset: Asset,
    status: String,
    filledAmount: Double,
    filledFee: Double,
    avgWeighedPrice: Double,
    eventTimestamp: Long,
    executedAmount: Option[Double],
    executedFee: Option[Double],
    executionPrice: Option[Double],
    totalExecutedPriceAssets: Option[Double]
  ): WsFullOrder = WsFullOrder(
    id,
    owner,
    timestamp,
    amountAsset,
    priceAsset,
    side,
    isMarket,
    price,
    amount,
    fee,
    feeAsset,
    status,
    filledAmount,
    filledFee,
    avgWeighedPrice,
    eventTimestamp,
    (executedAmount, executedFee, executionPrice, totalExecutedPriceAssets).mapN(WsExecutionInfo)
  )

  def wsUnapply(arg: WsFullOrder): Option[(
    Order.Id,
    Address,
    Long,
    Asset,
    Asset,
    OrderType,
    Boolean,
    Double,
    Double,
    Double,
    Asset,
    String,
    Double,
    Double,
    Double,
    Long,
    Option[Double],
    Option[Double],
    Option[Double],
    Option[Double]
  )] = {
    import arg._
    (
      id,
      owner,
      timestamp,
      amountAsset,
      priceAsset,
      side,
      isMarket,
      price,
      amount,
      fee,
      feeAsset,
      status,
      filledAmount,
      filledFee,
      avgWeighedPrice,
      eventTimestamp,
      executionInfo.map(_.amount),
      executionInfo.map(_.fee),
      executionInfo.map(_.price),
      executionInfo.map(_.totalPriceAssets)
    ).some
  }

  case class WsExecutionInfo(amount: Double, fee: Double, price: Double, totalPriceAssets: Double)

  def from(event: OrderCanceled)(implicit efc: ErrorFormatterContext): WsFullOrder = {
    val ao = event.acceptedOrder

    val amountAssetDecimals = efc.unsafeAssetDecimals(ao.order.assetPair.amountAsset)
    val feeAssetDecimals = efc.unsafeAssetDecimals(ao.order.feeAsset)
    val priceAssetDecimals = efc.unsafeAssetDecimals(ao.order.assetPair.priceAsset)

    def denormalizeAmount(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble
    def denormalizeFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, feeAssetDecimals).toDouble

    val fillingInfo = ao.fillingInfo
    WsFullOrder(
      id = ao.id,
      owner = ao.order.sender.toAddress,
      timestamp = ao.order.timestamp,
      amountAsset = ao.order.assetPair.amountAsset,
      priceAsset = ao.order.assetPair.priceAsset,
      side = ao.order.orderType,
      isMarket = ao.isMarket,
      price = denormalizePrice(ao.order.price),
      amount = denormalizeAmount(ao.order.amount),
      fee = denormalizeFee(ao.order.matcherFee),
      feeAsset = ao.order.feeAsset,
      status = OrderStatus.Cancelled.name,
      filledAmount = denormalizeAmount(fillingInfo.filledAmount),
      filledFee = denormalizeFee(fillingInfo.filledFee),
      avgWeighedPrice = denormalizePrice(fillingInfo.avgWeighedPrice),
      eventTimestamp = event.timestamp,
      executionInfo = none
    )
  }

  def from(
    ao: AcceptedOrder,
    event: OrderExecuted,
    denormalizeAmount: Long => Double,
    denormalizePrice: Long => Double,
    denormalizeFee: Long => Double
  )(implicit efc: ErrorFormatterContext): WsFullOrder = {
    val fillingInfo = ao.fillingInfo
    val executedFee = if (ao.id == event.counter.order.id()) event.counterExecutedFee else event.submittedExecutedFee

    WsFullOrder(
      id = ao.id,
      owner = ao.order.sender.toAddress,
      timestamp = ao.order.timestamp,
      amountAsset = ao.order.assetPair.amountAsset,
      priceAsset = ao.order.assetPair.priceAsset,
      side = ao.order.orderType,
      isMarket = ao.isMarket,
      price = denormalizePrice(ao.order.price),
      amount = denormalizeAmount(ao.order.amount),
      fee = denormalizeFee(ao.order.matcherFee),
      feeAsset = ao.order.feeAsset,
      status = ao.status.name,
      filledAmount = denormalizeAmount(fillingInfo.filledAmount),
      filledFee = denormalizeFee(fillingInfo.filledFee),
      avgWeighedPrice = denormalizePrice(fillingInfo.avgWeighedPrice),
      eventTimestamp = event.timestamp,
      executionInfo = WsExecutionInfo(
        amount = denormalizeAmount(event.executedAmount),
        fee = denormalizeFee(executedFee),
        price = denormalizePrice(event.executedPrice),
        totalPriceAssets = denormalizePrice(ao.fillingInfo.totalExecutedPriceAssets)
      ).some
    )
  }

  private val isMarketFormat: Format[Boolean] = Format(
    {
      case JsString("market") => JsSuccess(true)
      case JsString("limit") => JsSuccess(false)
      case _ => JsError("Cannot parse order type (market or limit)!")
    },
    if (_) JsString("market") else JsString("limit")
  )

  object Status extends Enumeration {
    type Status = Value
    val Filled, PartiallyFilled, Cancelled = Value
  }

  private val orderStatusFormat: Format[String] = Format(
    {
      case JsString(s) if s == Status.Filled.toString => JsSuccess(OrderStatus.Filled.name)
      case JsString(s) if s == Status.PartiallyFilled.toString => JsSuccess(OrderStatus.PartiallyFilled.name)
      case JsString(s) if s == Status.Cancelled.toString => JsSuccess(OrderStatus.Cancelled.name)
      case _ => JsError("Cannot parse order status")
    },
    {
      case OrderStatus.Filled.name => JsString(Status.Filled.toString)
      case OrderStatus.PartiallyFilled.name => JsString(Status.PartiallyFilled.toString)
      case OrderStatus.Cancelled.name => JsString(Status.Cancelled.toString)
      case _ => throw new IllegalArgumentException("Cannot parse order status")
    }
  )

  implicit val wsCompleteOrderFormat: Format[WsFullOrder] =
    (
      (__ \ "i").format[Order.Id] and // id
        (__ \ "o").format[Address] and // owner's address
        (__ \ "t").format[Long] and // timestamp
        (__ \ "A").format[Asset] and // amount asset
        (__ \ "P").format[Asset] and // price asset
        (__ \ "S").format[OrderType] and // side: BUY or SELL
        (__ \ "T").format[Boolean](isMarketFormat) and // type: MARKET or LIMIT
        (__ \ "p").format[Double](doubleAsStringFormat) and // price
        (__ \ "a").format[Double](doubleAsStringFormat) and // amount
        (__ \ "f").format[Double](doubleAsStringFormat) and // fee
        (__ \ "F").format[Asset] and // fee asset
        (__ \ "s").format[String](orderStatusFormat) and // status: FILLED or PARTIALLY_FILLED or CANCELLED
        (__ \ "q").format[Double](doubleAsStringFormat) and // filled amount
        (__ \ "Q").format[Double](doubleAsStringFormat) and // filled fee
        (__ \ "r").format[Double](doubleAsStringFormat) and // average weighed price
        (__ \ "Z").format[Long] and // event timestamp
        (__ \ "c").formatNullable[Double](doubleAsStringFormat) and // executed amount
        (__ \ "h").formatNullable[Double](doubleAsStringFormat) and // executed fee
        (__ \ "e").formatNullable[Double](doubleAsStringFormat) and // executed price
        (__ \ "E").formatNullable[Double](doubleAsStringFormat) // total executed amount of price asset
    )(WsFullOrder.apply, unlift(WsFullOrder.wsUnapply))

}
