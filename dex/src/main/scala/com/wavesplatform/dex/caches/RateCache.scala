package com.wavesplatform.dex.caches

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.dex.db.RateDB
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import org.iq80.leveldb.DB

import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._

trait RateCache {

  /** Adds or updates asset rate, returns previous rate value if there was one */
  def upsertRate(asset: Asset, value: Double): Option[Double]

  def getRate(asset: Asset): Option[Double]

  def getAllRates: Map[Asset, Double]

  /** Deletes asset rate, returns previous rate value if there was one */
  def deleteRate(asset: Asset): Option[Double]

}

object RateCache {

  private val WavesRate = Option(1d)

  def apply(db: DB): RateCache = new RateCache {

    private val rateDB  = RateDB(db)
    private val rateMap = new ConcurrentHashMap[IssuedAsset, Double](rateDB.getAllRates.asJava)

    def upsertRate(asset: Asset, value: Double): Option[Double] =
      asset.fold { WavesRate } { issuedAsset =>
        rateDB.upsertRate(issuedAsset, value)
        Option(rateMap.put(issuedAsset, value))
      }

    def getRate(asset: Asset): Option[Double] = asset.fold(WavesRate)(asset => Option(rateMap get asset))

    def getAllRates: Map[Asset, Double] = {
      rateMap.asScala.toMap.map { case (issuedAsset, value) => Asset.fromCompatId(issuedAsset.compatId) -> value } + (Waves -> 1d)
    }

    def deleteRate(asset: Asset): Option[Double] = asset.fold(WavesRate) { issuedAsset =>
      rateDB.deleteRate(issuedAsset)
      Option(rateMap.remove(issuedAsset))
    }
  }

  def inMem: RateCache = new RateCache {

    private val rates: TrieMap[Asset, Double] = TrieMap(Waves -> 1d)

    def upsertRate(asset: Asset, value: Double): Option[Double] = {
      asset.fold { WavesRate } { issuedAsset =>
        val previousValue = rates.get(issuedAsset)
        rates += (asset -> value)
        previousValue
      }
    }

    def getRate(asset: Asset): Option[Double] = rates.get(asset)
    def getAllRates: Map[Asset, Double]       = rates.toMap

    def deleteRate(asset: Asset): Option[Double] =
      asset.fold { Option(1d) } { issuedAsset =>
        val previousValue = rates.get(issuedAsset)
        rates -= issuedAsset
        previousValue
      }
  }
}
