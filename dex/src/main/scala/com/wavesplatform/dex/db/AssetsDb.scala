package com.wavesplatform.dex.db

import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

trait AssetsDb[F[_]] extends AssetsReadOnlyDb[F] {
  def put(asset: IssuedAsset, item: BriefAssetDescription): F[Unit]
}

object AssetsDb {

  def levelDb[F[_]](levelDb: LevelDb[F]): AssetsDb[F] = new AssetsDb[F] {
    override def put(asset: IssuedAsset, record: BriefAssetDescription): F[Unit] = levelDb.readWrite(_.put(DbKeys.asset(asset), Some(record)))
    override def get(asset: IssuedAsset): F[Option[BriefAssetDescription]] = levelDb.readOnly(_.get(DbKeys.asset(asset)))
  }

}
