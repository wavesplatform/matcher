package com.wavesplatform.it.api

import com.wavesplatform.dex.api.{ApiOrderBookHistoryItem, ApiV0OrderBook}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.it.api.responses.dex.{MarketStatusResponse, OrderStatusResponse}
import com.wavesplatform.dex.queue.QueueEventWithMeta

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[AssetPair, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, (ApiV0OrderBook, MarketStatusResponse)],
                        orderStatuses: Map[String, OrderStatusResponse],
                        orderTransactionIds: Map[String, Seq[String]],
                        reservedBalances: Map[KeyPair, Map[Asset, Long]],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[ApiOrderBookHistoryItem]]]) {
  override def toString: String =
    s"""MatcherState(
       |  offset=$offset,
       |  snapshots=$snapshots,
       |  orderBooks=$orderBooks,
       |  orderStatuses=$orderStatuses,
       |  orderTransactionIds=$orderTransactionIds,
       |  reservedBalances=$reservedBalances,
       |  orderHistory=$orderHistory
       |)""".stripMargin
}
