package com.wavesplatform.dex.api.ws.converters

import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.WsOrder
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.{AcceptedOrder, OrderStatus}

object WsOrderConverter {

  def fromDomain(ao: AcceptedOrder)(implicit efc: ErrorFormatterContext): WsOrder = fromDomain(ao, ao.status)

  def fromDomain(ao: AcceptedOrder, status: OrderStatus)(implicit efc: ErrorFormatterContext): WsOrder = {

    val amountAssetDecimals = efc.unsafeAssetDecimals(ao.order.assetPair.amountAsset)
    val feeAssetDecimals = efc.unsafeAssetDecimals(ao.order.feeAsset)
    val priceAssetDecimals = efc.unsafeAssetDecimals(ao.order.assetPair.priceAsset)

    def denormalizeAmount(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizeFee(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, feeAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    WsOrder(
      ao.id,
      ao.order.assetPair.amountAsset,
      ao.order.assetPair.priceAsset,
      ao.feeAsset,
      ao.order.orderType,
      ao.order.timestamp.some,
      ao.isMarket.some,
      ao.price.some.map(denormalizePrice),
      ao.order.amount.some.map(denormalizeAmount),
      ao.order.matcherFee.some.map(denormalizeFee),
      status.name.some,
      ao.fillingInfo.filledAmount.some.map(denormalizeAmount),
      ao.fillingInfo.filledFee.some.map(denormalizeFee),
      ao.fillingInfo.avgWeighedPrice.some.map(denormalizePrice),
      ao.fillingInfo.totalExecutedPriceAssets.some.map(denormalizePrice)
    )
  }

}
