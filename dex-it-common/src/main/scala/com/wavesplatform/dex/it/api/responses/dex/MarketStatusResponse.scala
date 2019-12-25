package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class MarketStatusResponse(lastPrice: Option[Long],
                                lastSide: Option[String],
                                bid: Option[Long],
                                bidAmount: Option[Long],
                                ask: Option[Long],
                                askAmount: Option[Long])
object MarketStatusResponse {
  implicit val format: Format[MarketStatusResponse] = Json.format
}
