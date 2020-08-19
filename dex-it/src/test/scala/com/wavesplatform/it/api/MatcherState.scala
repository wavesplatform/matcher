package com.wavesplatform.it.api

import com.wavesplatform.dex.api.http.entities._
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair

case class MatcherState(offset: HttpOffset,
                        snapshots: HttpSnapshotOffsets,
                        orderBooks: Map[AssetPair, (HttpV0OrderBook, HttpMarketStatus)],
                        orderStatuses: Map[String, HttpOrderStatus],
                        orderTransactionIds: Map[String, Set[String]],
                        reservedBalances: Map[KeyPair, HttpBalance],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[HttpOrderBookHistoryItem]]]) {
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
