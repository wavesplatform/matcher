package com.wavesplatform.it.api.dex

import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.it.json._
import play.api.libs.json.{Format, Json}

case class OrderBookHistoryItem(id: Order.Id,
                                `type`: String,
                                amount: Long,
                                price: Long,
                                timestamp: Long,
                                filled: Long,
                                status: OrderStatus,
                                assetPair: AssetPair)

object OrderBookHistoryItem {
  implicit val orderbookHistory: Format[OrderBookHistoryItem] = Json.format
}
