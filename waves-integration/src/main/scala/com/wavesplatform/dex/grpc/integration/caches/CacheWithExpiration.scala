package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import cats.Applicative
import com.google.common.cache.{Cache, CacheBuilder}

abstract class CacheWithExpiration[F[_], K <: AnyRef, V <: AnyRef](load: K => F[V], expiration: Duration)(implicit ap: Applicative[F]) {

  lazy private val cache: Cache[K, V] = CacheBuilder.newBuilder.expireAfterWrite(expiration).build[K, V]

  def get(key: K): F[V] = {
    Option { cache.getIfPresent(key) }.fold {
      ap.map { load(key) } { balance =>
        put(key, balance)
        balance
      }
    }(ap.pure)
  }

  def put(key: K, value: V): Unit = cache.put(key, value)

  def size: Long = cache.size()
}
