package com.wavesplatform.it.api

import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair

case class MatcherState(offset: ApiOffset,
                        snapshots: ApiSnapshotOffsets,
                        orderBooks: Map[AssetPair, (ApiV0OrderBook, ApiMarketStatus)],
                        orderStatuses: Map[String, ApiOrderStatus],
                        orderTransactionIds: Map[String, Seq[String]],
                        reservedBalances: Map[KeyPair, ApiBalance],
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
