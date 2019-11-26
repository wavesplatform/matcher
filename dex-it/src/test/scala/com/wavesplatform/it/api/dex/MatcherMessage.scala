package com.wavesplatform.it.api.dex

import play.api.libs.json.{Format, Json}

case class MatcherMessage(id: String)
object MatcherMessage {
  implicit val format: Format[MatcherMessage] = Json.format
}
