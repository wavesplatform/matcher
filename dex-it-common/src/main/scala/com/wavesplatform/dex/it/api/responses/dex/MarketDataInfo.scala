package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class MarketDataInfo(matcherPublicKey: String, markets: Seq[MarketData])
object MarketDataInfo {
  implicit val marketDataInfoResponseFormat: Format[MarketDataInfo] = Json.format
}
