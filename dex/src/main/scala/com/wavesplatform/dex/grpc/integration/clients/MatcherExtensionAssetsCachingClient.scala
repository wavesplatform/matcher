package com.wavesplatform.dex.grpc.integration.clients

import cats.instances.future._
import cats.syntax.apply._
import com.wavesplatform.dex.db.AssetsCache
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream.Status
import com.wavesplatform.dex.grpc.integration.clients.domain.{AddressBalanceUpdates, WavesNodeUpdates}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}

class MatcherExtensionAssetsCachingClient(
  underlying: WavesBlockchainClient,
  assetsCache: AssetsCache
)(implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient
    with ScorexLogging {

  override def partialBalancesSnapshot(address: Address, assets: Set[Asset]): Future[AddressBalanceUpdates] =
    saveAssetsDescription(assets) *> underlying.partialBalancesSnapshot(address, assets)

  override def fullBalancesSnapshot(address: Address, excludeAssets: Set[Asset]): Future[AddressBalanceUpdates] =
    for {
      xs <- underlying.fullBalancesSnapshot(address, excludeAssets)
      _ <- saveAssetsDescription(xs.regular.keySet ++ xs.pessimisticCorrection.keySet)
    } yield xs

  override lazy val updates: Observable[(WavesNodeUpdates, Boolean)] = underlying
    .updates
    .mapEval { update =>
      val assets = update._1.balanceUpdates.valuesIterator.flatMap(x => x.regular.keysIterator ++ x.pessimisticCorrection.keysIterator).toSet
      Task.fromFuture(saveAssetsDescription(assets)).map(_ => update)
    }

  override def isFeatureActivated(id: Short): Future[Boolean] = underlying.isFeatureActivated(id)

  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] =
    for {
      cached <- assetsCache.get(asset)
      desc <- if (cached.isEmpty) underlying.assetDescription(asset) else Future.successful(None)
      _ <- desc match {
        case None => Future.unit
        case Some(desc) => assetsCache.put(asset, desc)
      }
    } yield cached.orElse(desc)

  override def hasScript(asset: IssuedAsset): Future[Boolean] = underlying.hasScript(asset)

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] = underlying.hasScript(address)

  override def runScript(address: Address, input: Order): Future[RunScriptResult] = underlying.runScript(address, input)

  override def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = underlying.areKnown(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[BroadcastResult] = underlying.broadcastTx(tx)

  override def checkedBroadcastTx(tx: ExchangeTransaction): Future[CheckedBroadcastResult] = underlying.checkedBroadcastTx(tx)

  override def isOrderConfirmed(orderId: ByteStr): Future[Boolean] = underlying.isOrderConfirmed(orderId)

  override def close(): Future[Unit] = underlying.close()

  private def saveAssetsDescription(assets: Set[Asset]): Future[Unit] = {
    val issuedAssets = assets.iterator.collect {
      case asset: IssuedAsset => asset
    }

    Future
      .traverse(issuedAssets) { asset =>
        for {
          contains <- assetsCache.contains(asset)
          _ <- if (contains) Future.unit else assetDescription(asset)
        } yield ()
      }
      .map(_ => ())
  }

  override def status(): Status = underlying.status()
}
