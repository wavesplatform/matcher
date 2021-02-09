package com.wavesplatform.dex.db

import java.util.concurrent.ConcurrentHashMap

import cats.Applicative
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.option._
import com.wavesplatform.dex.db.leveldb.LevelDb
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

// TODO tests
trait AssetsStorage[F[_]] {
  def put(asset: IssuedAsset, item: BriefAssetDescription): F[Unit]
  def get(asset: IssuedAsset): F[Option[BriefAssetDescription]]

  def putAll(xs: Map[IssuedAsset, BriefAssetDescription]): F[Unit]
  def contained(xs: Set[IssuedAsset]): F[Set[IssuedAsset]]
}

object AssetsStorage {

  def levelDB[F[_]](levelDb: LevelDb[F]): AssetsStorage[F] = new AssetsStorage[F] {
    override def put(asset: IssuedAsset, record: BriefAssetDescription): F[Unit] = levelDb.readWrite(_.put(DbKeys.asset(asset), Some(record)))
    override def get(asset: IssuedAsset): F[Option[BriefAssetDescription]] = levelDb.readOnly(_.get(DbKeys.asset(asset)))

    override def putAll(xs: Map[IssuedAsset, BriefAssetDescription]): F[Unit] = levelDb.readWrite { rw =>
      xs.foreach { case (asset, x) => rw.put(DbKeys.asset(asset), x.some) }
    }

    override def contained(xs: Set[IssuedAsset]): F[Set[IssuedAsset]] = levelDb.readOnly { ro =>
      xs.filter(x => ro.get(DbKeys.asset(x)).nonEmpty)
    }

  }

  def inMem[F[_]: Applicative]: AssetsStorage[F] = new AssetsStorage[F] {
    private val assetsCache = new ConcurrentHashMap[Asset, BriefAssetDescription]
    override def put(asset: IssuedAsset, item: BriefAssetDescription): F[Unit] = assetsCache.putIfAbsent(asset, item).pure[F].as(())
    override def get(asset: IssuedAsset): F[Option[BriefAssetDescription]] = Option(assetsCache get asset).pure[F]
    override def putAll(xs: Map[IssuedAsset, BriefAssetDescription]): F[Unit] = xs.foreach(Function.tupled(assetsCache.put)).pure[F]
    override def contained(xs: Set[IssuedAsset]): F[Set[IssuedAsset]] = xs.filter(assetsCache.containsKey).pure[F]
  }

  implicit final class Ops[F[_]: Applicative](val self: AssetsStorage[F]) {
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
