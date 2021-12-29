package com.wavesplatform.dex.api.ws.converters

import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.WsFullOrder
import com.wavesplatform.dex.api.ws.entities.WsFullOrder.WsExecutionInfo
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.{OrderCanceled, OrderExecuted}
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}

object WsFullOrderConverter {

  def toWs(event: OrderCanceled)(implicit efc: ErrorFormatterContext): WsFullOrder = {
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

  def toWs(
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

}
