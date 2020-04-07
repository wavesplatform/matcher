package com.wavesplatform.dex.settings

import com.wavesplatform.dex.api.websockets.actors.PingPongHandlerActor
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.duration.FiniteDuration

final case class WebSocketSettings(messagesInterval: FiniteDuration,
                                   maxConnectionLifetime: FiniteDuration,
                                   pingPongSettings: PingPongHandlerActor.Settings)

object WebSocketSettings {

  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase
}
