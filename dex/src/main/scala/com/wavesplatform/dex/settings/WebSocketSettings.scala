package com.wavesplatform.dex.settings

import com.wavesplatform.dex.api.ws.actors.{WsExternalClientHandlerActor, WsInternalBroadcastActor, WsInternalClientHandlerActor}

final case class WebSocketSettings(
  externalClientHandler: WsExternalClientHandlerActor.Settings,
  internalBroadcast: WsInternalBroadcastActor.Settings,
  internalClientHandler: WsInternalClientHandlerActor.Settings
)
