package com.wavesplatform.dex.api.http.entities

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.dex.domain.asset.AssetPair
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads}

case class HttpV0OrderBook(
  @ApiModelProperty(value = "Timestamp of the last Order Book update") timestamp: Long,
  @ApiModelProperty(value = "Corresponding Asset Pair") pair: AssetPair,
  @ApiModelProperty(value = "List of aggregated bid levels") bids: List[HttpV0LevelAgg],
  @ApiModelProperty(value = "List of aggregated ask levels") asks: List[HttpV0LevelAgg]
)

object HttpV0OrderBook {

  implicit val httpV0OrderBookReads: Reads[HttpV0OrderBook] = Json.reads

  def fromHttpResponse(response: HttpResponse): HttpV0OrderBook =
    Json.parse(response.entity.asInstanceOf[HttpEntity.Strict].getData().decodeString(StandardCharsets.UTF_8)).as[HttpV0OrderBook]

}
