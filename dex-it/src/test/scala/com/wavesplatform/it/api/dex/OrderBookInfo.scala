package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class OrderBookInfo(matchingRules: MatchingRules, restrictions: Option[OrderRestrictions])
object OrderBookInfo {
  implicit val orderBookInfo: Format[OrderBookInfo] = Json.format
}
