package com.wavesplatform.dex.api.http.converters

import com.wavesplatform.dex.api.http.entities.HttpOrderBookHistoryItem
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.{OrderInfo, OrderStatus}

object HttpOrderBookHistoryItemConverter {

  def fromOrderInfo(id: Order.Id, info: OrderInfo[OrderStatus]): HttpOrderBookHistoryItem = HttpOrderBookHistoryItem(
    id = id,
    `type` = info.side,
    orderType = info.orderType,
    amount = info.amount,
    filled = info.status.filledAmount,
    price = info.price,
    fee = info.matcherFee,
    filledFee = info.status.filledFee,
    feeAsset = info.feeAsset,
    timestamp = info.timestamp,
    status = info.status.name,
    assetPair = info.assetPair,
    avgWeighedPrice = info.avgWeighedPrice,
    version = info.orderVersion,
    totalExecutedPriceAssets = info.totalExecutedPriceAssets
  )

}
