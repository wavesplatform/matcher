package com.wavesplatform.dex.grpc.integration.clients

import cats.instances.future._
import cats.syntax.apply._
import com.wavesplatform.dex.db.AssetsStorage
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}

class WavesBlockchainAssetsWatchingClient(
  settings: WavesBlockchainClientSettings,
  underlying: WavesBlockchainClient[Future],
  assetsStorage: AssetsStorage
)(implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainCachingClient(underlying, settings.defaultCachesExpiration) {

  // Do not use
  override def realTimeBalanceChanges: Observable[WavesBlockchainClient.BalanceChanges] =
    realTimeBalanceBatchChanges.flatMap(Observable.fromIterable)

  // TODO Refactor to fit this method
  def realTimeBalanceBatchChanges: Observable[List[WavesBlockchainClient.BalanceChanges]] =
    underlying.realTimeBalanceChanges
      .bufferIntrospective(settings.balanceStreamBufferSize)
      .mapEval { xs =>
        val assets = xs.map(_.asset).iterator
        Task.fromFuture(saveAssetsDescription(assets)).map(_ => xs)
      }

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    saveAssetsDescription(assets.iterator) *> underlying.spendableBalances(address, assets)

  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    for {
      xs <- underlying.allAssetsSpendableBalance(address)
      _ <- saveAssetsDescription(xs.keysIterator)
    } yield xs

  private def saveAssetsDescription(assets: Iterator[Asset]) =
    Future.traverse(assets.collect { case asset: IssuedAsset if !assetsStorage.contains(asset) => asset }.toSet)(saveAssetDescription)

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
