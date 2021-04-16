package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

abstract class AssetsCache(implicit ec: ExecutionContext) {
  val cached: AssetsStorage[Id]

  def contains(asset: Asset): Future[Boolean] = get(asset).map(_.nonEmpty)
  def get(asset: Asset): Future[Option[BriefAssetDescription]]
  def put(asset: Asset, item: BriefAssetDescription): Future[Unit]
}

object AssetsCache {

  def from(storage: AssetsStorage[Future])(implicit ec: ExecutionContext): AssetsCache =
    new AssetsCache with ScorexLogging {
      private val assetsCache = new ConcurrentHashMap[Asset, BriefAssetDescription]

      override val cached = new AssetsStorage[Id] {

        override def put(asset: Asset.IssuedAsset, item: BriefAssetDescription): Unit =
          throw new IllegalAccessException("This method should not be called, use AssetsCache.put!")

        override def get(asset: Asset.IssuedAsset): Option[BriefAssetDescription] = Option(assetsCache.get(asset))
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
