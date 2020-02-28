package com.wavesplatform.dex.settings

import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.duration.FiniteDuration

final case class WebSocketSettings(messagesInterval: FiniteDuration)

object WebSocketSettings {

  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase
}
