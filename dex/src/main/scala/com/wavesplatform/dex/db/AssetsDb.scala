package com.wavesplatform.dex.db

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

trait AssetsDb[F[_]] {
  def put(asset: IssuedAsset, item: BriefAssetDescription): F[Unit]
  def get(asset: IssuedAsset): F[Option[BriefAssetDescription]]
}

object AssetsDb {

  def levelDb[F[_]](levelDb: LevelDb[F]): AssetsDb[F] = new AssetsDb[F] {
    override def put(asset: IssuedAsset, record: BriefAssetDescription): F[Unit] = levelDb.readWrite(_.put(DbKeys.asset(asset), Some(record)))
    override def get(asset: IssuedAsset): F[Option[BriefAssetDescription]] = levelDb.readOnly(_.get(DbKeys.asset(asset)))
  }

  implicit final class Ops[F[_]: Applicative](val self: AssetsDb[F]) {
    def contains(asset: IssuedAsset): F[Boolean] = self.get(asset).map(_.nonEmpty)

    def get(asset: Asset): F[Option[BriefAssetDescription]] = asset match {
      case asset: IssuedAsset => self.get(asset)
      case Asset.Waves => BriefAssetDescription.someWavesDescription.pure[F]
    }

    def unsafeGet(asset: Asset): F[BriefAssetDescription] =
      get(asset).map(_.getOrElse(throw new RuntimeException(s"Unknown asset: ${asset.toString}")))

    def unsafeGetDecimals(asset: Asset): F[Int] = unsafeGet(asset).map(_.decimals)
    def unsafeGetHasScript(asset: Asset): F[Boolean] = unsafeGet(asset).map(_.hasScript)
  }

}
