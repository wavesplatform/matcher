package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.{AcceptedOrderType, OrderInfo, OrderStatus}
import play.api.libs.json.{Json, OFormat}

// TODO
// 1. Add id to OrderInfo
// 2. Remove this class
case class ApiOrderBookHistoryItem(id: Order.Id,
                                   `type`: OrderType,
                                   orderType: AcceptedOrderType,
                                   amount: Long,
                                   filled: Long,
                                   price: Long,
                                   fee: Long,
                                   filledFee: Long,
                                   feeAsset: Asset,
                                   timestamp: Long,
                                   status: String,
                                   assetPair: AssetPair,
                                   avgWeighedPrice: Long)

object ApiOrderBookHistoryItem {

  implicit val orderBookHistoryItemFormat: OFormat[ApiOrderBookHistoryItem] = Json.format

  def fromOrderInfo(id: Order.Id, info: OrderInfo[OrderStatus]): ApiOrderBookHistoryItem = ApiOrderBookHistoryItem(
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
    avgWeighedPrice = info.avgWeighedPrice
  )
}
