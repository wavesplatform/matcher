package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.meta.getSimpleName
import com.wavesplatform.dex.tool.OnComplete

import scala.collection.mutable.ListBuffer

trait RateDb[F[_]] {

  def upsertRate(asset: IssuedAsset, value: Double): F[Unit]

  def getAllRates: F[Map[IssuedAsset, Double]]

  def deleteRate(asset: IssuedAsset): F[Unit]
}

object RateDb {

  private val cls = getSimpleName(this)

  def apply[F[_]: OnComplete](levelDb: LevelDb[F]): RateDb[F] = new RateDb[F] {

    def upsertRate(asset: IssuedAsset, value: Double): F[Unit] =
      measureDb(cls, "upsertRate") {
        levelDb.put(DbKeys.rate(asset), value)
      }

    def getAllRates: F[Map[IssuedAsset, Double]] =
      measureDb(cls, "getAllRates") {
        levelDb.readOnly { ro =>
          val ratesListBuffer = ListBuffer[(IssuedAsset, Double)]()
          ro.iterateOver(DbKeys.ratePrefix) { dbEntry =>
            val asset = IssuedAsset(ByteStr(dbEntry.getKey.drop(2)))
            val value = DbKeys.rate(asset).parse(dbEntry.getValue)
            ratesListBuffer.append(asset -> value)
          }
          ratesListBuffer.toMap
        }
      }

    def deleteRate(asset: IssuedAsset): F[Unit] =
      measureDb(cls, "deleteRate") {
        levelDb.delete(DbKeys.rate(asset))
      }

  }

}
