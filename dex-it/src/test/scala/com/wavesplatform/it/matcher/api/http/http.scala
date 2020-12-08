package com.wavesplatform.it.matcher.api

import com.wavesplatform.dex.api.http.entities.HttpOrderBookHistoryItem
import com.wavesplatform.dex.asset.DoubleOps.NumericOps
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.{AcceptedOrderType, OrderStatus}

package object http {

  def toHttpOrderBookHistoryItem(
    order: Order,
    status: OrderStatus,
    fee: Long = 0.003.waves,
    acceptedOrderType: AcceptedOrderType = AcceptedOrderType.Limit,
    avgWeighedPrice: Long = 0,
    totalExecutedPriceAssets: Long = 0
  ): HttpOrderBookHistoryItem =
    HttpOrderBookHistoryItem(
      order.id(),
      order.orderType,
      acceptedOrderType,
      order.amount,
      status.filledAmount,
      order.price,
      fee,
      status.filledFee,
      order.feeAsset,
      order.timestamp,
      status.name,
      order.assetPair,
      avgWeighedPrice,
      order.version,
      totalExecutedPriceAssets
    )

}
