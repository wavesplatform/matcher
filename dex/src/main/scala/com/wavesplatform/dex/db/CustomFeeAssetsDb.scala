package com.wavesplatform.dex.db

import com.google.common.primitives.Longs
import com.wavesplatform.dex.caches.OrderFeeSettingsCache.AssetsActionForOffset
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.meta.getSimpleName
import com.wavesplatform.dex.tool.OnComplete

trait CustomFeeAssetsDb[F[_]] {
  def all(): F[Set[AssetsActionForOffset]]
  def save(action: AssetsActionForOffset): F[Unit]
}

object CustomFeeAssetsDb {

  private val cls = getSimpleName(this)

  def levelDb[F[_]: OnComplete](levelDb: LevelDb[F]): CustomFeeAssetsDb[F] = new CustomFeeAssetsDb[F] {

    def all(): F[Set[AssetsActionForOffset]] =
      measureDb(cls, "all") {
        levelDb.readOnly { ro =>
          val r = Set.newBuilder[AssetsActionForOffset]

          ro.iterateOver(DbKeys.FeeAssetsPrefix) { dbEntry =>
            val offset = Longs.fromByteArray(dbEntry.getKey.drop(2))
            val parsedData = DbKeys.customFeeAsset(offset).parse(dbEntry.getValue)
            parsedData.foreach(v => r.addOne(v))
          }

          r.result()
        }
      }

    def save(action: AssetsActionForOffset): F[Unit] =
      measureDb(cls, "save") {
        levelDb.readWrite { rw =>
          val k = DbKeys.customFeeAsset(action.offset)
          if (!rw.has(k))
            rw.put(k, Some(action))
        }
      }

  }

}
