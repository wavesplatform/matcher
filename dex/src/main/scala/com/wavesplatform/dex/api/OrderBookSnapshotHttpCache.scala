package com.wavesplatform.dex.api

import java.util.concurrent.ScheduledFuture

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import cats.data.OptionT
import cats.implicits._
import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.wavesplatform.dex.api.OrderBookSnapshotHttpCache.Settings
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized, Normalized}
import com.wavesplatform.dex.model.{OrderBook, OrderBookResult}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.Time
import kamon.Kamon

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class OrderBookSnapshotHttpCache(settings: Settings,
                                 time: Time,
                                 assetDecimals: Asset => Future[Option[Int]],
                                 orderBookSnapshot: AssetPair => Option[OrderBook.AggregatedSnapshot])(implicit ec: ExecutionContext)
    extends AutoCloseable {
  import OrderBookSnapshotHttpCache._

  private val depthRanges = settings.depthRanges.sorted

  private val orderBookSnapshotCache =
    CacheBuilder
      .newBuilder()
      .expireAfterAccess(settings.cacheTimeout.length, settings.cacheTimeout.unit)
      .build(
        new CacheLoader[Key, Future[HttpResponse]] {

          override def load(key: Key): Future[HttpResponse] = {

            def getAssetDecimals(asset: Asset): OptionT[Future, Int] = OptionT { assetDecimals(asset) }

            val assetPairDecimals: OptionT[Future, (Int, Int)] = key.format match {
              case Denormalized =>
                for {
                  maybeAmountAssetDecimals <- getAssetDecimals(key.pair.amountAsset)
                  maybePriceAssetDecimals  <- getAssetDecimals(key.pair.priceAsset)
                } yield maybeAmountAssetDecimals -> maybePriceAssetDecimals
              case _ => OptionT.none
            }

            assetPairDecimals.value.map { pairDecimals =>
              val orderBook = orderBookSnapshot(key.pair) getOrElse { OrderBook.AggregatedSnapshot() }

              val entity =
                OrderBookResult(
                  time.correctedTime(),
                  key.pair,
                  orderBook.bids.take(key.depth),
                  orderBook.asks.take(key.depth),
                  pairDecimals
                )

              HttpResponse(
                entity = HttpEntity(
                  ContentTypes.`application/json`,
                  OrderBookResult.toJson(entity)
                )
              )
            }
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

  def get(pair: AssetPair, depth: Option[Int], format: DecimalsFormat = Normalized): Future[HttpResponse] = {
    orderBookSnapshotCache.get(
      Key(pair, settings.nearestBigger(depth), format)
    )
  }

  def invalidate(pair: AssetPair): Unit = {
    orderBookSnapshotCache.invalidateAll {
      depthRanges
        .flatMap(depth => List(Normalized, Denormalized).map(depth -> _))
        .map { case (depth, format) => Key(pair, depth, format) }
        .asJava
    }
  }

  override def close(): Unit = {
    statsScheduler.cancel(true)
  }
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
