package com.wavesplatform.dex.it.api.responses.dex

import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.model.AcceptedOrderType
import play.api.libs.json.{Format, Json}

case class OrderBookHistoryItem(id: Order.Id,
                                `type`: String,
                                orderType: AcceptedOrderType,
                                amount: Long,
                                filled: Long,
                                price: Long,
                                fee: Long,
                                filledFee: Long,
                                feeAsset: Asset,
                                timestamp: Long,
                                status: OrderStatus,
                                assetPair: AssetPair)

object OrderBookHistoryItem {
  implicit val orderbookHistory: Format[OrderBookHistoryItem] = Json.format
}
