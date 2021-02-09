package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

abstract class AssetsStorageCache(implicit ec: ExecutionContext) {
  val cached: AssetsStorage[Id]

  def contains(asset: Asset): Future[Boolean] = get(asset).map(_.nonEmpty)
  def get(asset: Asset): Future[Option[BriefAssetDescription]]
  def put(asset: Asset, item: BriefAssetDescription): Future[Unit]
}

object AssetsStorageCache {

  def from(storage: AssetsStorage[Future])(implicit ec: ExecutionContext): AssetsStorageCache =
    new AssetsStorageCache with ScorexLogging {
      private val assetsCache = new ConcurrentHashMap[Asset, BriefAssetDescription]

      override val cached = new AssetsStorage[Id] {

        // TODO REMOVE?
        // We don't check assetCache here before put, because it is a responsibility of a user.
        override def put(asset: Asset.IssuedAsset, item: BriefAssetDescription): Unit = {
          storage.put(asset, item).onComplete {
            case Success(_) =>
            case Failure(e) => log.error(s"Can't save asset $asset with $item", e)
          }
          assetsCache.put(asset, item)
        }

        override def get(asset: Asset.IssuedAsset): Option[BriefAssetDescription] = Option(assetsCache.get(asset))

        override def putAll(xs: Map[IssuedAsset, BriefAssetDescription]): Unit = {
          storage.putAll(xs)
          xs.foreach(Function.tupled(assetsCache.put))
        }

        override def contained(xs: Set[IssuedAsset]): Set[IssuedAsset] = xs.filter(assetsCache.containsKey)
      }

      override def get(asset: Asset): Future[Option[BriefAssetDescription]] = asset match {
        case asset: Asset.IssuedAsset =>
          cached.get(asset) match {
            case None =>
              storage
                .get(asset)
                .andThen {
                  case Success(Some(x)) => cached.put(asset, x)
                }

            case x => Future.successful(x)
          }

        case _ => Future.successful(Some(BriefAssetDescription.wavesDescription))
      }

      override def put(asset: Asset, item: BriefAssetDescription): Future[Unit] = asset match {
        case asset: Asset.IssuedAsset =>
          cached.put(asset, item)
          storage.put(asset, item)

        case _ => Future.unit
      }

    }

}
