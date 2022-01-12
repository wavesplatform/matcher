package com.wavesplatform.dex.api.ws.converters

import cats.data.NonEmptyList
import com.wavesplatform.dex.api.ws.entities.WsFullOrder
import com.wavesplatform.dex.api.ws.protocol.WsOrdersUpdate
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.AcceptedOrder
import com.wavesplatform.dex.model.Events.{OrderCanceled, OrderExecuted}

object WsOrderUpdateConverter {

  def toWs(x: OrderCanceled)(implicit efc: ErrorFormatterContext): WsOrdersUpdate = WsOrdersUpdate(
    NonEmptyList.one(WsFullOrderConverter.toWs(x))
  )

  def toWs(x: OrderExecuted, ts: Long)(implicit efc: ErrorFormatterContext): WsOrdersUpdate = {
    val ao1 = x.counterRemaining
    val assetPair = ao1.order.assetPair

    val amountAssetDecimals = efc.unsafeAssetDecimals(assetPair.amountAsset)
    val priceAssetDecimals = efc.unsafeAssetDecimals(assetPair.priceAsset)

    def denormalizeAmount(value: Long): Double = Denormalization.denormalizeAmountAndFee(value, amountAssetDecimals).toDouble
    def denormalizePrice(value: Long): Double = Denormalization.denormalizePrice(value, amountAssetDecimals, priceAssetDecimals).toDouble

    def from(ao: AcceptedOrder): WsFullOrder =
      WsFullOrderConverter.toWs(
        ao,
        x,
        denormalizeAmount,
        denormalizePrice,
        Denormalization.denormalizeAmountAndFee(_, efc.unsafeAssetDecimals(ao.feeAsset)).toDouble
      )

    WsOrdersUpdate(NonEmptyList.of(ao1, x.submittedRemaining).map(from), timestamp = ts)
  }

}
