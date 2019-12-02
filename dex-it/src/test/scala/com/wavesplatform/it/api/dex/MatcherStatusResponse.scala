package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class MatcherStatusResponse(status: String, filledAmount: Option[Long])
object MatcherStatusResponse {
  implicit val format: Format[MatcherStatusResponse] = Json.format
}
