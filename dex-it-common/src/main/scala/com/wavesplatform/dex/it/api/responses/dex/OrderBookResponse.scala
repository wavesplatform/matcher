package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class OrderBookResponse(timestamp: Long, pair: PairResponse, bids: List[LevelResponse], asks: List[LevelResponse])
object OrderBookResponse {
  implicit val format: Format[OrderBookResponse] = Json.format
}
