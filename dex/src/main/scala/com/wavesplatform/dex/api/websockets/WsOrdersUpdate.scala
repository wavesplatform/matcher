package com.wavesplatform.dex.api.websockets

import cats.syntax.option._
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{ExchangeTransactionCreated, OrderCanceled}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class WsOrdersUpdate(orders: List[WsCompleteOrder], timestamp: Long = System.currentTimeMillis) extends WsServerMessage {
  override val tpe: String = WsOrdersUpdate.tpe

  def append(other: WsOrdersUpdate): WsOrdersUpdate = {
    // prepend to list 
  }
}

object WsOrdersUpdate {

  val tpe = "osu"

  def from(x: OrderCanceled)(implicit efc: ErrorFormatterContext): WsOrdersUpdate = {
    val ao = x.acceptedOrder

    val amountAssetDecimals = efc.assetDecimals(ao.order.assetPair.amountAsset)
    val priceAssetDecimals  = efc.assetDecimals(ao.order.assetPair.priceAsset)

    def denormalizeAmountAndFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double        = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    WsOrdersUpdate(
      WsCompleteOrder(
        id = ao.id,
        timestamp = ao.order.timestamp,
        amountAsset = ao.order.assetPair.amountAsset,
        priceAsset = ao.order.assetPair.priceAsset,
        side = ao.order.orderType,
        isMarket = ao.isMarket,
        price = denormalizePrice(ao.order.price),
        amount = denormalizeAmountAndFee(ao.order.amount),
        fee = denormalizeAmountAndFee(ao.order.matcherFee),
        feeAsset = ao.order.feeAsset,
        status = "",
        filledAmount = denormalizeAmountAndFee(ao.order.amount), // TODO test
        filledFee = denormalizeAmountAndFee(ao.order.matcherFee), // TODO test
        avgWeighedPrice = denormalizePrice(ao.avgWeighedPriceNominator),
        eventTimestamp = x.timestamp,
        executedAmount = denormalizeAmountAndFee(x.reason.executedAmount).some,
        executedFee = denormalizeAmountAndFee(x.reason.counterExecutedFee).some,
        executedPrice = denormalizePrice(x.reason.executedPrice).some
      )
    )
  }


  def from(x: ExchangeTransactionCreated)(implicit efc: ErrorFormatterContext): WsOrdersUpdate = {
    val ao1 = x.reason.counter
    val ao1Remaining = x.reason.counterRemaining

    val amountAssetDecimals = efc.assetDecimals(ao1.order.assetPair.amountAsset)
    val priceAssetDecimals  = efc.assetDecimals(ao1.order.assetPair.priceAsset)

    def denormalizeAmountAndFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double        = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    val completeOrder1 = WsCompleteOrder(
      id = ao1.id,
      timestamp = ao1.order.timestamp,
      amountAsset = ao1.order.assetPair.amountAsset,
      priceAsset = ao1.order.assetPair.priceAsset,
      side = ao1.order.orderType,
      isMarket = ao1.isMarket,
      price = denormalizePrice(ao1.order.price),
      amount = denormalizeAmountAndFee(ao1.order.amount),
      fee = denormalizeAmountAndFee(ao1.order.matcherFee),
      feeAsset = ao1.order.feeAsset,
      status = "",
      filledAmount = denormalizeAmountAndFee(ao1.order.amount - ao1Remaining.amount), // TODO test
      filledFee = denormalizeAmountAndFee(ao1.order.matcherFee - ao1Remaining.fee), // TODO test
      avgWeighedPrice = denormalizePrice(ao1.avgWeighedPriceNominator),
      eventTimestamp = x.reason.timestamp,
      executedAmount = denormalizeAmountAndFee(x.reason.executedAmount).some,
      executedFee = denormalizeAmountAndFee(x.reason.counterExecutedFee).some,
      executedPrice = denormalizePrice(x.reason.executedPrice).some
    )

    val ao2 = x.reason.submitted
    val ao2Remaining = x.reason.submittedRemaining

    val completeOrder2 = WsCompleteOrder(
      id = ao2.id,
      timestamp = ao2.order.timestamp,
      amountAsset = ao2.order.assetPair.amountAsset,
      priceAsset = ao2.order.assetPair.priceAsset,
      side = ao2.order.orderType,
      isMarket = ao2.isMarket,
      price = denormalizePrice(ao2.order.price),
      amount = denormalizeAmountAndFee(ao2.order.amount),
      fee = denormalizeAmountAndFee(ao2.order.matcherFee),
      feeAsset = ao2.order.feeAsset,
      status = "",
      filledAmount = denormalizeAmountAndFee(ao2.order.amount - ao2Remaining.amount), // TODO test
      filledFee = denormalizeAmountAndFee(ao2.order.matcherFee - ao2Remaining.fee), // TODO test
      avgWeighedPrice = denormalizePrice(ao2.avgWeighedPriceNominator),
      eventTimestamp = x.reason.timestamp,
      executedAmount = denormalizeAmountAndFee(x.reason.executedAmount).some,
      executedFee = denormalizeAmountAndFee(x.reason.counterExecutedFee).some,
      executedPrice = denormalizePrice(x.reason.executedPrice).some
    )

    WsOrdersUpdate(
      List(completeOrder1, completeOrder2)
    )
  }

  def wsUnapply(arg: WsOrdersUpdate): Option[(String, Long, List[WsCompleteOrder])] = (arg.tpe, arg.timestamp, arg.orders).some

  implicit val wsOrdersUpdateFormat: Format[WsOrdersUpdate] = (
    (__ \ "T").format[String] and
      (__ \ "_").format[Long] and
      (__ \ "o").format[List[WsCompleteOrder]]
  )(
    (_, ts, maybeOrders) => WsOrdersUpdate(maybeOrders, ts),
    unlift(WsOrdersUpdate.wsUnapply)
  )
}
