package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class MatchingRules(tickSize: String)
object MatchingRules {
  implicit val matchingRules: Format[MatchingRules] = Json.format
}
