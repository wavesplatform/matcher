package com.wavesplatform.it.api

import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, (OrderBookResponse, MarketStatusResponse)],
                        orderStatuses: Map[String, OrderStatusResponse],
                        reservedBalances: Map[KeyPair, Map[Asset, Long]],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[OrderBookHistoryItem]]])
