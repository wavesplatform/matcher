package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr

import scala.collection.mutable.ListBuffer

trait RateDb[F[_]] {

  def upsertRate(asset: IssuedAsset, value: Double): F[Unit]

  def getAllRates: F[Map[IssuedAsset, Double]]

  def deleteRate(asset: IssuedAsset): F[Unit]
}

object RateDb {

  def apply[F[_]](levelDb: LevelDb[F]): RateDb[F] = new RateDb[F] {

    def upsertRate(asset: IssuedAsset, value: Double): F[Unit] = levelDb.readWrite(_.put(DbKeys.rate(asset), value))

    def getAllRates: F[Map[IssuedAsset, Double]] =
      levelDb.readOnly { ro =>
        val ratesListBuffer = ListBuffer[(IssuedAsset, Double)]()
        ro.iterateOver(DbKeys.ratePrefix) { dbEntry =>
          val asset = IssuedAsset(ByteStr(dbEntry.getKey.drop(2)))
          val value = DbKeys.rate(asset).parse(dbEntry.getValue)
          ratesListBuffer.append(asset -> value)
        }
        ratesListBuffer.toMap
      }

    def deleteRate(asset: IssuedAsset): F[Unit] = levelDb.readWrite(_.delete(DbKeys.rate(asset)))
  }

}
