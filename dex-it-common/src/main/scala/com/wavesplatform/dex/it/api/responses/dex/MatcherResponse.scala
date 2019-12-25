package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class MatcherResponse(status: String, message: MatcherMessage)
object MatcherResponse {
  implicit val format: Format[MatcherResponse] = Json.format
}
