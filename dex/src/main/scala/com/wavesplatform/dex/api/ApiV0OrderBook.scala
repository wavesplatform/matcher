package com.wavesplatform.dex.api

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.model.LevelAgg
import play.api.libs.json.{Json, OFormat}

case class ApiV0OrderBook(timestamp: Long, pair: AssetPair, bids: List[LevelAgg], asks: List[LevelAgg])

object ApiV0OrderBook {

  implicit val apiV0OrderBookFormat: OFormat[ApiV0OrderBook] = Json.format

  def fromHttpResponse(response: HttpResponse): ApiV0OrderBook =
    Json.parse(response.entity.asInstanceOf[HttpEntity.Strict].getData().decodeString(StandardCharsets.UTF_8)).as[ApiV0OrderBook]
}
