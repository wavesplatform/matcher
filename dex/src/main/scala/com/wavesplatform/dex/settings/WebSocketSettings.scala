package com.wavesplatform.dex.settings

import com.wavesplatform.dex.api.websockets.actors.WsHandlerActor
import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.duration.FiniteDuration

final case class WebSocketSettings(messagesInterval: FiniteDuration,
                                   maxConnectionLifetime: FiniteDuration,
                                   webSocketHandler: WsHandlerActor.Settings)

object WebSocketSettings {

  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase
}
