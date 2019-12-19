package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class MatchingRules(tickSize: String)
object MatchingRules {
  implicit val matchingRules: Format[MatchingRules] = Json.format
}
