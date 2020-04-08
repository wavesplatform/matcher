package com.wavesplatform.dex.settings

import com.wavesplatform.dex.api.websockets.actors.SystemMessagesHandlerActor
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.duration.FiniteDuration

final case class WebSocketSettings(messagesInterval: FiniteDuration,
                                   maxConnectionLifetime: FiniteDuration,
                                   systemMessagesSettings: SystemMessagesHandlerActor.Settings)

object WebSocketSettings {

  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase
}
