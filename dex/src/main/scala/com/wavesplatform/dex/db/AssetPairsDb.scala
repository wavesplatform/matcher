package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.AssetPair

trait AssetPairsDb[F[_]] {
  def add(pair: AssetPair): F[Unit]
  def remove(pair: AssetPair): F[Unit]
  def all(): F[Set[AssetPair]]
}

object AssetPairsDb {

  def levelDb[F[_]](levelDb: LevelDb[F]): AssetPairsDb[F] = new AssetPairsDb[F] {

    def add(pair: AssetPair): F[Unit] = levelDb.readWrite(_.put(DbKeys.assetPair(pair), ()))
    def remove(pair: AssetPair): F[Unit] = levelDb.readWrite(_.delete(DbKeys.assetPair(pair)))

    def all(): F[Set[AssetPair]] = levelDb.readOnly { ro =>
      val r = Set.newBuilder[AssetPair]

      ro.iterateOver(DbKeys.AssetPairsPrefix) { pair =>
        r += AssetPair.fromBytes(pair.getKey.drop(2))._1
      }

      r.result()
    }

  }

}
