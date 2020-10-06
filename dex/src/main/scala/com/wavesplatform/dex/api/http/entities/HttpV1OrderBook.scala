package com.wavesplatform.dex.api.http.entities

import java.nio.charset.StandardCharsets

import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads}

case class HttpV1OrderBook(
  @ApiModelProperty(value = "Timestamp of the last Order Book update") timestamp: Long,
  @ApiModelProperty(
    value = "List of aggregated denormalized bid levels [price, amount]",
    dataType = "[[Ljava.lang.String;",
    example = """[ [ "1.18", "43800.00000000" ], [ "1.17", "52187.00000000" ], [ "1.16", "809.00000000" ] ]"""
  ) bids: List[HttpV1LevelAgg],
  @ApiModelProperty(
    value = "List of aggregated denormalized ask levels [price, amount]",
    dataType = "[[Ljava.lang.String;",
    example = """[ [ "1.19", "2134.00000000" ], [ "1.20", "747.00000000" ] ]"""
  ) asks: List[HttpV1LevelAgg]
)

object HttpV1OrderBook {

  implicit val httpV1OrderBookReads: Reads[HttpV1OrderBook] = Json.reads

  def fromHttpResponse(response: HttpResponse): HttpV1OrderBook =
    Json.parse(response.entity.asInstanceOf[HttpEntity.Strict].getData().decodeString(StandardCharsets.UTF_8)).as[HttpV1OrderBook]

}
