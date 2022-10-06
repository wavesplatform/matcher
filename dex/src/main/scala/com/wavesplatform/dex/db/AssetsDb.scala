package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

trait AssetsDb[F[_]] extends AssetsReadOnlyDb[F] {
  def put(asset: IssuedAsset, item: BriefAssetDescription): F[Unit]
}

object AssetsDb {

  private val cls = getClass.getSimpleName.filter(_ != '$')

  def levelDb[F[_]: OnComplete](levelDb: LevelDb[F]): AssetsDb[F] = new AssetsDb[F] {

    override def put(asset: IssuedAsset, record: BriefAssetDescription): F[Unit] =
      measureDb(cls, "put") {
        levelDb.put(DbKeys.asset(asset), Some(record))
      }

    override def get(asset: IssuedAsset): F[Option[BriefAssetDescription]] =
      measureDb(cls, "get") {
        levelDb.get(DbKeys.asset(asset))
      }

  }

}
