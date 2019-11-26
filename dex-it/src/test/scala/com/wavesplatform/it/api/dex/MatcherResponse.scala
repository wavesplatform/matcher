package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class MatcherResponse(status: String, message: MatcherMessage)
object MatcherResponse {
  implicit val format: Format[MatcherResponse] = Json.format
}
