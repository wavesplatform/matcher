package com.wavesplatform.dex.db

import com.google.common.primitives.Longs
import com.wavesplatform.dex.caches.OrderFeeSettingsCache.AssetsActionForOffset
import com.wavesplatform.dex.db.leveldb.LevelDb

trait CustomFeeAssetsDb[F[_]] {
  def all(): F[Set[AssetsActionForOffset]]
  def save(action: AssetsActionForOffset): F[Unit]
}

object CustomFeeAssetsDb {

  def levelDb[F[_]](levelDb: LevelDb[F]): CustomFeeAssetsDb[F] = new CustomFeeAssetsDb[F] {

    def all(): F[Set[AssetsActionForOffset]] = levelDb.readOnly { ro =>
      val r = Set.newBuilder[AssetsActionForOffset]

      ro.iterateOver(DbKeys.FeeAssetsPrefix) { dbEntry =>
        val offset = Longs.fromByteArray(dbEntry.getKey.drop(2))
        val parsedData = DbKeys.customFeeAsset(offset).parse(dbEntry.getValue)
        parsedData.foreach(v => r.addOne(v))
      }

      r.result()
    }

    def save(action: AssetsActionForOffset): F[Unit] = levelDb.readWrite { rw =>
      val k = DbKeys.customFeeAsset(action.offset)
      if (!rw.has(k))
        rw.put(k, Some(action))
    }

  }

}
