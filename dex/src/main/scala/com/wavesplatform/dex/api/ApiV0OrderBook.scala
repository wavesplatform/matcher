package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.LevelAgg
import play.api.libs.json.{Json, OFormat}

case class ApiV0OrderBook(timestamp: Long, pair: AssetPair, bids: List[LevelAgg], asks: List[LevelAgg])

object ApiV0OrderBook {
  implicit val apiV0OrderBookFormat: OFormat[ApiV0OrderBook] = Json.format
}
