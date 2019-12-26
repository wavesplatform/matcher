package com.wavesplatform.dex.it.api.responses.dex

import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.dex.it.json._
import com.wavesplatform.dex.model.AcceptedOrderType
import com.wavesplatform.transaction.Asset
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
