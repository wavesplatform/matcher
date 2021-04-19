package com.wavesplatform.dex.db

import cats.Id
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

abstract class AssetsCache(implicit ec: ExecutionContext) {
  val cached: AssetsReadOnlyDb[Id]

  def contains(asset: Asset): Future[Boolean] = get(asset).map(_.nonEmpty)
  def get(asset: Asset): Future[Option[BriefAssetDescription]]
  def put(asset: Asset, item: BriefAssetDescription): Future[Unit]
}

object AssetsCache {

  def from(storage: AssetsDb[Future])(implicit ec: ExecutionContext): AssetsCache =
    new AssetsCache with ScorexLogging {
      private val assetsCache = new ConcurrentHashMap[Asset, BriefAssetDescription]

      override val cached = (asset: Asset.IssuedAsset) => Option(assetsCache.get(asset))

      override def get(asset: Asset): Future[Option[BriefAssetDescription]] = asset match {
        case asset: Asset.IssuedAsset =>
          cached.get(asset) match {
            case None =>
              // We don't need to fall back to blockchain client here, because we guarantee all assets will be cached retrieved during:
              //   1. An asset pair validation in AssetPairBuilder
              //   2. An order validation in ValidationStages.mkFirst
              //   2. Consuming from a queue in Application.consumeMessages
              // So all input assets will be saved in memory cache before they will be used.
              //
              // What if the cache failed? It will fail
              //   1. AssetPairBuilder.validateAssetId, so we won't allow to access this order book
              //   2. ValidationStages.mkFirst, so the order won't process
              //   3. Application.consumeMessages, so we'll stop the application
              //
              // We will refactor this later and guarantee availability of this information.
              storage
                .get(asset)
                .andThen {
                  case Success(Some(x)) => assetsCache.put(asset, x)
                  case Failure(e) => log.error(s"Can't get $asset", e)
                }

            case x => Future.successful(x)
          }

        case _ => Future.successful(Some(BriefAssetDescription.wavesDescription))
      }

      override def put(asset: Asset, item: BriefAssetDescription): Future[Unit] = asset match {
        case asset: Asset.IssuedAsset =>
          assetsCache.put(asset, item)
          storage.put(asset, item).andThen {
            case Failure(e) => log.error(s"Can't save $asset with $item", e)
          }

        case _ => Future.unit
      }

    }

}
