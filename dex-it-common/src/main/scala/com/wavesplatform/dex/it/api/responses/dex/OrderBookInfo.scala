package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class OrderBookInfo(matchingRules: MatchingRules, restrictions: Option[OrderRestrictions])
object OrderBookInfo {
  implicit val orderBookInfo: Format[OrderBookInfo] = Json.format
}
