package com.wavesplatform.dex.settings

import com.wavesplatform.dex.api.ws.actors.{WsExternalClientHandlerActor, WsInternalBroadcastActor, WsInternalClientHandlerActor}
import net.ceedubs.ficus.readers.NameMapper

final case class WebSocketSettings(externalClientHandler: WsExternalClientHandlerActor.Settings,
                                   internalBroadcast: WsInternalBroadcastActor.Settings,
                                   internalClientHandler: WsInternalClientHandlerActor.Settings)

object WebSocketSettings {
  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase
}
