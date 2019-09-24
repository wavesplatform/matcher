package com.wavesplatform.it.api

import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.queue.QueueEventWithMeta
import com.wavesplatform.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, (OrderBookResponse, MarketStatusResponse)],
                        orderStatuses: Map[String, MatcherStatusResponseWithFee],
                        reservedBalances: Map[KeyPair, Map[String, Long]],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[OrderHistory]]])
