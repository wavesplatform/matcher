package com.wavesplatform.dex.api.http.entities

case class HttpMarketStatus(
  lastTrade: Option[HttpLastTrade],
  bestBid: Option[HttpLevelAgg],
  bestAsk: Option[HttpLevelAgg]
)
