package com.wavesplatform.dex.api

import java.util.concurrent.ScheduledFuture

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.dex.api.OrderBookSnapshotHttpCache.Settings
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized, Normalized}
import com.wavesplatform.dex.model.{OrderBook, OrderBookResult}
import com.wavesplatform.dex.time.Time
import kamon.Kamon

import scala.collection.JavaConverters._
import scala.concurrent.duration._

class OrderBookSnapshotHttpCache(settings: Settings,
                                 time: Time,
                                 assetDecimals: Asset => Option[Int],
                                 orderBookSnapshot: AssetPair => Option[OrderBook.AggregatedSnapshot])
    extends AutoCloseable {
  import OrderBookSnapshotHttpCache._

  private val depthRanges = settings.depthRanges.sorted

  private val orderBookSnapshotCache =
    CacheBuilder
      .newBuilder()
      .expireAfterAccess(settings.cacheTimeout.length, settings.cacheTimeout.unit)
      .build(
        new CacheLoader[Key, HttpResponse] {

          override def load(key: Key): HttpResponse = {

            val assetPairDecimals = key.format match {
              case Denormalized =>
                for {
                  ad <- assetDecimals(key.pair.amountAsset)
                  pd <- assetDecimals(key.pair.priceAsset)
                } yield ad -> pd
              case _ => None
            }

            val orderBook = orderBookSnapshot(key.pair) getOrElse { OrderBook.AggregatedSnapshot() }

            val entity =
              OrderBookResult(
                time.correctedTime(),
                key.pair,
                orderBook.bids.take(key.depth),
                orderBook.asks.take(key.depth),
                assetPairDecimals
              )

            HttpResponse(
              entity = HttpEntity(
                ContentTypes.`application/json`,
                OrderBookResult.toJson(entity)
              )
            )
          }
        }
      )

  private val statsScheduler: ScheduledFuture[_] = {

    val period       = 3.seconds
    val requestStats = Kamon.histogram("matcher.http.ob.cache.req")
    val hitStats     = Kamon.histogram("matcher.http.ob.cache.hit")

    Kamon
      .scheduler()
      .scheduleWithFixedDelay(
        { () =>
          val stats = orderBookSnapshotCache.stats()
          requestStats.record(stats.requestCount())
          hitStats.record((stats.hitRate() * 100).toLong)
        },
        period.toSeconds,
        period.toSeconds,
        period.unit
      )
  }

  def get(pair: AssetPair, depth: Option[Int], format: DecimalsFormat = Normalized): HttpResponse = {
    orderBookSnapshotCache.get { Key(pair, settings.nearestBigger(depth), format) }
  }

  def invalidate(pair: AssetPair): Unit = {
    orderBookSnapshotCache.invalidateAll {
      depthRanges
        .flatMap(depth => List(Normalized, Denormalized).map(depth -> _))
        .map { case (depth, format) => Key(pair, depth, format) }
        .asJava
    }
  }

  override def close(): Unit = statsScheduler.cancel(true)
}

object OrderBookSnapshotHttpCache {

  case class Settings(cacheTimeout: FiniteDuration, depthRanges: List[Int], defaultDepth: Option[Int]) {
    def nearestBigger(to: Option[Int]): Int =
      to.orElse(defaultDepth)
        .flatMap(desiredDepth => depthRanges.find(_ >= desiredDepth))
        .getOrElse(depthRanges.max)
  }

  private case class Key(pair: AssetPair, depth: Int, format: DecimalsFormat)
}
