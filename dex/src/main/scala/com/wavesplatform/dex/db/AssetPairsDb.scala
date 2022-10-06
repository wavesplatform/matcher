package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.meta.getSimpleName
import com.wavesplatform.dex.tool.OnComplete

trait AssetPairsDb[F[_]] {
  def add(pair: AssetPair): F[Unit]
  def remove(pair: AssetPair): F[Unit]
  def all(): F[Set[AssetPair]]
  def contains(pair: AssetPair): F[Boolean]
}

object AssetPairsDb {

  private val cls = getSimpleName(this)

  def levelDb[F[_]: OnComplete](levelDb: LevelDb[F]): AssetPairsDb[F] = new AssetPairsDb[F] {

    def add(pair: AssetPair): F[Unit] =
      measureDb(cls, "add") {
        levelDb.put(DbKeys.assetPair(pair), ())
      }

    def remove(pair: AssetPair): F[Unit] =
      measureDb(cls, "remove") {
        levelDb.delete(DbKeys.assetPair(pair))
      }

    def all(): F[Set[AssetPair]] =
      measureDb(cls, "all") {
        levelDb.readOnly { ro =>
          val r = Set.newBuilder[AssetPair]

          ro.iterateOver(DbKeys.AssetPairsPrefix) { pair =>
            r += AssetPair.fromBytes(pair.getKey.drop(2))._1
          }

          r.result()
        }
      }

    def contains(pair: AssetPair): F[Boolean] =
      measureDb(cls, "contains") {
        levelDb.has(DbKeys.assetPair(pair))
      }

  }

}
