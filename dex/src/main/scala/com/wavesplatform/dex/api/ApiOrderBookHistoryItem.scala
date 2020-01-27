package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.model.AcceptedOrderType
import play.api.libs.json.{Format, Json}

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
                                   assetPair: AssetPair)

object ApiOrderBookHistoryItem {
  implicit val orderBookHistoryItemFormat: Format[ApiOrderBookHistoryItem] = Json.format
}
