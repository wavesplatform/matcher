package com.wavesplatform.dex.redis

import com.wavesplatform.dex.settings.RedisSettings
import org.redisson.Redisson
import org.redisson.client.codec.Codec
import org.redisson.config.{Config => RedisConfig}

final class RedisClient(settings: RedisSettings) extends AutoCloseable {

  private val cfg = new RedisConfig()
  cfg.useSingleServer()
    .setAddress(settings.address)
    .setUsername(settings.username)
    .setPassword(settings.password)
    .setRetryAttempts(settings.retryAttempts)
    .setRetryInterval(settings.retryInterval)
    .setKeepAlive(settings.keepAlive)
    .setPingConnectionInterval(settings.pingConnectionInterval)
    .setConnectionMinimumIdleSize(settings.connectionMinimumIdleSize)
    .setConnectionPoolSize(settings.connectionPoolSize)

  cfg.setThreads(settings.threads)
  cfg.setNettyThreads(settings.nettyThreads)

  private val redisson = Redisson.create(cfg)

  def getStream[K, V](streamName: String, codec: Codec): RedisStream[K, V] =
    new RedisStream(redisson.getStream[K, V](streamName, codec))

  def shutdown(): Unit = redisson.shutdown()

  def close(): Unit = shutdown()

}
