package com.wavesplatform.dex.api.http.entities

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.dex.domain.asset.AssetPair
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads}

case class ApiV0OrderBook(
    @ApiModelProperty(value = "Timestamp of the last Order Book update") timestamp: Long,
    @ApiModelProperty(value = "Corresponding Asset Pair") pair: AssetPair,
    @ApiModelProperty(value = "List of aggregated bid levels") bids: List[ApiV0LevelAgg],
    @ApiModelProperty(value = "List of aggregated ask levels") asks: List[ApiV0LevelAgg]
)

object ApiV0OrderBook {

  implicit val apiV0OrderBookReads: Reads[ApiV0OrderBook] = Json.reads

  def fromHttpResponse(response: HttpResponse): ApiV0OrderBook =
    Json.parse(response.entity.asInstanceOf[HttpEntity.Strict].getData().decodeString(StandardCharsets.UTF_8)).as[ApiV0OrderBook]
}
