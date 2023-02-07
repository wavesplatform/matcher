package com.wavesplatform.dex.settings

final case class RedisInternalClientHandlerActorSettings(
  enabled: Boolean,
  streamName: String,
  key: String
)
