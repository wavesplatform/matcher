package com.wavesplatform.dex.settings

final case class RedisSettings(
  address: String,
  username: String,
  password: String,
  nettyThreads: Int,
  threads: Int,
  retryAttempts: Int,
  retryInterval: Int,
  keepAlive: Boolean,
  pingConnectionInterval: Int,
  connectionPoolSize: Int,
  connectionMinimumIdleSize: Int
)
