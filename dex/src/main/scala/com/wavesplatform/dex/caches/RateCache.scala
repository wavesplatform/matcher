package com.wavesplatform.dex.caches

import java.util.concurrent.atomic.AtomicReference

import com.wavesplatform.dex.db.RateDB
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import org.iq80.leveldb.DB

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
  def apply(rateDB: RateDB): RateCache = new RateCache {

    private val xs: AtomicReference[RateCacheMaps] =
      new AtomicReference(RateCacheMaps(rateDB.getAllRates.toMap[Asset, Double].updated(Waves, WavesRate)))

    def upsertRate(asset: Asset, value: Double): Option[Double] =
      asset.fold(WavesRateOpt) { asset =>
        rateDB.upsertRate(asset, value)
        xs.getAndUpdate(_.upsert(asset, value)).latest.get(asset)
      }

    /**
     * @return The least rate, because a client could not update the rate in time, which leads to rejection
     */
    def getRate(asset: Asset): Option[Double] = asset.fold(WavesRateOpt)(xs.get.least)

    def getAllRates: Map[Asset, Double] = xs.get.latest

    def deleteRate(asset: Asset): Option[Double] = asset.fold(WavesRateOpt) { asset =>
      rateDB.deleteRate(asset)
      xs.getAndUpdate(_.delete(asset)).latest.get(asset)
    }

  }

  def apply(db: DB): RateCache = RateCache(RateDB(db))

  def inMem: RateCache = apply(RateDB.inMem)

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
