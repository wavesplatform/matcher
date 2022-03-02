package com.wavesplatform.dex.db

import cats.Functor
import cats.syntax.functor._
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr

trait RateDb[F[_]] {

  def upsertRate(asset: IssuedAsset, value: Double): F[Unit]

  def getAllRates: F[Map[IssuedAsset, Double]]

  def deleteRate(asset: IssuedAsset): F[Unit]
}

object RateDb {

  def apply[F[_]: Functor](levelDb: LevelDb[F]): RateDb[F] = new RateDb[F] {

    def upsertRate(asset: IssuedAsset, value: Double): F[Unit] = levelDb.put(DbKeys.rate(asset), value)

    def getAllRates: F[Map[IssuedAsset, Double]] =
      levelDb.scanOver(DbKeys.ratePrefix)(Map.newBuilder[IssuedAsset, Double]) { (ratesListBuffer, entry) =>
        val asset = IssuedAsset(ByteStr(entry.getKey.drop(2)))
        val value = DbKeys.rate(asset).parse(entry.getValue)

        ratesListBuffer += (asset -> value)
        ratesListBuffer
      }.map(_.result())

    def deleteRate(asset: IssuedAsset): F[Unit] = levelDb.delete(DbKeys.rate(asset))
  }

}
