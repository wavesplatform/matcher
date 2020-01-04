package com.wavesplatform.dex.grpc.integration.caches

import java.time.Duration

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.dex.domain.utils.ScorexLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  * Common logic for caching of the blockchain access results
  *
  * @param loader blockchain access function
  * @param expiration living time of the each cache entry
  * @param invalidationPredicate custom logic for value invalidation after it's loading to the cache
  */
abstract class BlockchainCache[K <: AnyRef, V <: AnyRef](loader: K => Future[V], expiration: Option[Duration], invalidationPredicate: V => Boolean)(
    implicit ec: ExecutionContext)
    extends ScorexLogging {

  lazy private val cache: LoadingCache[K, Future[V]] = {
    val builder = CacheBuilder.newBuilder
    expiration
      .fold(builder)(builder.expireAfterWrite)
      .build {
        new CacheLoader[K, Future[V]] {
          override def load(key: K): Future[V] = loader(key) andThen {
            case Success(value) if invalidationPredicate(value) => cache.invalidate(key)
            case Failure(exception)                             => log.error(s"Error while value loading occurred: ", exception); cache.invalidate(key)
          }
        }
      }
  }

  def get(key: K): Future[V] = cache.get(key)

  def put(key: K, value: Future[V]): Unit = cache.put(key, value)
}

object BlockchainCache {

  def noCustomInvalidationLogic[V](value: V): Boolean = false
}
