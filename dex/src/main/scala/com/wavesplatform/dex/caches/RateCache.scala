package com.wavesplatform.dex.caches

import java.util.concurrent.atomic.AtomicReference
import com.wavesplatform.dex.db.RateDb
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.utils.ScorexLogging

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait RateCache {

  /** Adds or updates asset rate, returns previous rate value if there was one */
  def upsertRate(asset: Asset, value: Double): Option[Double]

  def getRate(asset: Asset): Option[Double]

  def getAllRates: Map[Asset, Double]

  /** Deletes asset rate, returns previous rate value if there was one */
  def deleteRate(asset: Asset): Option[Double]

}

object RateCache {

  private val WavesRate = 1d
  private val WavesRateOpt = Option(WavesRate)

  /**
   * 1. Stores internally two values for each asset
   * 2. getRate returns the least rate
   * 3. other methods return the latest rate
   */
  def apply(rateDb: RateDb[Future])(implicit ec: ExecutionContext): Future[RateCache] = {
    def mkRateCache(allRates: Map[Asset.IssuedAsset, Double]): RateCache =
      new RateCache with ScorexLogging {
        private val xs: AtomicReference[RateCacheMaps] =
          new AtomicReference(RateCacheMaps(allRates.toMap[Asset, Double].updated(Waves, WavesRate)))

        def upsertRate(asset: Asset, value: Double): Option[Double] =
          asset.fold(WavesRateOpt) { asset =>
            rateDb.upsertRate(asset, value).onComplete {
              case Success(_) =>
              case Failure(th) =>
                log.error(s"error while upserting rate for asset: ${asset.toString}, value: $value", th)
            }
            xs.getAndUpdate(_.upsert(asset, value)).latest.get(asset)
          }

        /**
         * @return The least rate, because a client could not update the rate in time, which leads to rejection
         */
        def getRate(asset: Asset): Option[Double] = asset.fold(WavesRateOpt)(xs.get.least)

        def getAllRates: Map[Asset, Double] = xs.get.latest

        def deleteRate(asset: Asset): Option[Double] = asset.fold(WavesRateOpt) { asset =>
          rateDb.deleteRate(asset).onComplete {
            case Success(_) =>
            case Failure(th) =>
              log.error(s"error while deleting rate for asset: ${asset.toString}", th)
          }
          xs.getAndUpdate(_.delete(asset)).latest.get(asset)
        }
      }

    for {
      allRates <- rateDb.getAllRates
      rateCache = mkRateCache(allRates)
    } yield rateCache
  }

  def apply(levelDb: LevelDb[Future])(implicit ec: ExecutionContext): Future[RateCache] =
    RateCache(RateDb(levelDb))

  private case class RateCacheMaps(latest: Map[Asset, Double], lastTwo: Map[Asset, DropOldestFixedBuffer2]) {

    def upsert(asset: Asset, value: Double): RateCacheMaps = {
      val orig = lastTwo.get(asset)
      val updated = orig.fold(DropOldestFixedBuffer2(value))(_.append(value))
      RateCacheMaps(
        latest = latest.updated(asset, value),
        lastTwo = lastTwo.updated(asset, updated)
      )
    }

    def delete(asset: Asset): RateCacheMaps = RateCacheMaps(latest - asset, lastTwo - asset)
    def least(asset: Asset): Option[Double] = lastTwo.get(asset).map(_.min)

  }

  private object RateCacheMaps {

    def apply(init: Map[Asset, Double]): RateCacheMaps = RateCacheMaps(
      latest = init,
      lastTwo = init.map { case (k, v) => k -> DropOldestFixedBuffer2(v) }
    )

  }

}
