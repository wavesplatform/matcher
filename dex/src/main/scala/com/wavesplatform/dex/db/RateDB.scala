package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.DBExt
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import org.iq80.leveldb.DB

import scala.collection.mutable.ListBuffer

trait RateDB {

  def upsertRate(asset: IssuedAsset, value: Double): Unit

  def getAllRates: Map[IssuedAsset, Double]

  def deleteRate(asset: IssuedAsset): Unit
}

object RateDB {

  def apply(db: DB): RateDB = new RateDB {

    def upsertRate(asset: IssuedAsset, value: Double): Unit = db.readWrite { _.put(DbKeys.rate(asset), value) }

    def getAllRates: Map[IssuedAsset, Double] = {

      val ratesListBuffer = ListBuffer[(IssuedAsset, Double)]()

      db.iterateOver(DbKeys.ratePrefix) { dbEntry =>
        val asset = IssuedAsset(ByteStr(dbEntry.getKey.drop(2)))
        val value = DbKeys.rate(asset).parse(dbEntry.getValue)
        ratesListBuffer.append(asset -> value)
      }

      ratesListBuffer.toMap
    }

    def deleteRate(asset: IssuedAsset): Unit = db.readWrite { _.delete(DbKeys.rate(asset)) }
  }
}
