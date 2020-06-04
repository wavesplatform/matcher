package com.wavesplatform.dex.api

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.wavesplatform.dex.domain.asset.AssetPair
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, OFormat}

case class ApiV0OrderBook(
    @ApiModelProperty(value = "Timestamp of the last Order Book update") timestamp: Long,
    @ApiModelProperty(value = "Corresponding Asset Pair") pair: AssetPair,
    @ApiModelProperty(value = "List of aggregated bid levels") bids: List[ApiLevelAgg],
    @ApiModelProperty(value = "List of aggregated ask levels") asks: List[ApiLevelAgg]
)

object ApiV0OrderBook {

  implicit val apiV0OrderBookFormat: OFormat[ApiV0OrderBook] = Json.format

  def fromHttpResponse(response: HttpResponse): ApiV0OrderBook =
    Json.parse(response.entity.asInstanceOf[HttpEntity.Strict].getData().decodeString(StandardCharsets.UTF_8)).as[ApiV0OrderBook]
}
