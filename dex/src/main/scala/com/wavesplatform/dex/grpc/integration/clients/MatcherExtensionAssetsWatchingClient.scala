package com.wavesplatform.dex.grpc.integration.clients

import cats.instances.future._
import cats.syntax.apply._
import com.wavesplatform.dex.db.AssetsStorage
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}

class MatcherExtensionAssetsWatchingClient(
  settings: WavesBlockchainClientSettings, // TODO remove
  underlying: WavesBlockchainClient,
  assetsStorage: AssetsStorage
)(implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient
    with ScorexLogging {

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    saveAssetsDescription(assets) *> underlying.spendableBalances(address, assets)

  override def allAssetsSpendableBalance(address: Address, excludeAssets: Set[Asset]): Future[Map[Asset, Long]] =
    for {
      xs <- underlying.allAssetsSpendableBalance(address, excludeAssets)
      _ <- saveAssetsDescription(xs.keySet)
    } yield xs

  override def updates: Observable[WavesNodeUpdates] = underlying
    .updates
    .mapEval { xs =>
      val assets = xs.updatedBalances.valuesIterator.flatMap(_.keysIterator).toSet
      Task.fromFuture(saveAssetsDescription(assets)).map(_ => xs)
    }

  override def isFeatureActivated(id: Short): Future[Boolean] = underlying.isFeatureActivated(id)

  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] = underlying.assetDescription(asset)

  override def hasScript(asset: IssuedAsset): Future[Boolean] = underlying.hasScript(asset)

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] = underlying.hasScript(address)

  override def runScript(address: Address, input: Order): Future[RunScriptResult] = underlying.runScript(address, input)

  override def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = underlying.areKnown(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[BroadcastResult] = underlying.broadcastTx(tx)

  override def checkedBroadcastTx(tx: ExchangeTransaction): Future[CheckedBroadcastResult] = underlying.checkedBroadcastTx(tx)

  override def isOrderConfirmed(orderId: ByteStr): Future[Boolean] = underlying.isOrderConfirmed(orderId)

  override def close(): Future[Unit] = underlying.close()

  private def saveAssetsDescription(assets: Set[Asset]): Future[Unit] =
    Future.traverse(assets.iterator.collect { case asset: IssuedAsset if !assetsStorage.contains(asset) => asset })(saveAssetDescription).map(
      _ => ()
    )

  private def saveAssetDescription(asset: IssuedAsset): Future[Unit] =
    assetsStorage.get(asset) match {
      case Some(_) => Future.unit
      case None =>
        assetDescription(asset).map {
          case Some(x) => assetsStorage.put(asset, x)
          case None => log.warn(s"Can't find the '$asset' asset in the blockchain")
        }
    }

}
