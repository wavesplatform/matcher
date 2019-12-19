package com.wavesplatform.dex.it.api.responses.dex

import play.api.libs.json.{Format, Json}

case class MatcherMessage(id: String)
object MatcherMessage {
  implicit val format: Format[MatcherMessage] = Json.format
}
