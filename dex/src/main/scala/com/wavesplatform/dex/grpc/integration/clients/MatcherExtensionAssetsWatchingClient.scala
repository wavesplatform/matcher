package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress

import cats.Monoid
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
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import monix.eval.Task
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}

class MatcherExtensionAssetsWatchingClient(
  settings: WavesBlockchainClientSettings,
  underlying: WavesBlockchainClient,
  assetsStorage: AssetsStorage
)(implicit grpcExecutionContext: ExecutionContext)
    extends WavesBlockchainClient
    with ScorexLogging {

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    saveAssetsDescription(assets.iterator) *> underlying.spendableBalances(address, assets)

  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    for {
      xs <- underlying.allAssetsSpendableBalance(address)
      _ <- saveAssetsDescription(xs.keysIterator)
    } yield xs

  override def updates: Observable[WavesBlockchainClient.Updates] = underlying
    .updates
    .bufferIntrospective(settings.balanceStreamBufferSize)
    .mapEval { xs =>
      val r = Monoid.combineAll(xs)
      val assets = r.updatedBalances.valuesIterator.flatMap(_.keysIterator)
      Task.fromFuture(saveAssetsDescription(assets)).map(_ => r)
    }

  override def isFeatureActivated(id: Short): Future[Boolean] = underlying.isFeatureActivated(id)

  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] = underlying.assetDescription(asset)

  override def hasScript(asset: IssuedAsset): Future[Boolean] = underlying.hasScript(asset)

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] = underlying.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] = underlying.hasScript(address)

  override def runScript(address: Address, input: Order): Future[RunScriptResult] = underlying.runScript(address, input)

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] = underlying.wereForged(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean] = underlying.broadcastTx(tx)

  override def forgedOrder(orderId: ByteStr): Future[Boolean] = underlying.forgedOrder(orderId)

  override def getNodeAddress: Future[InetAddress] = underlying.getNodeAddress

  override def close(): Future[Unit] = underlying.close()

  private def saveAssetsDescription(assets: Iterator[Asset]): Future[Unit] =
    Future.traverse(assets.collect { case asset: IssuedAsset if !assetsStorage.contains(asset) => asset }.toSet)(saveAssetDescription).map(_ => ())

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
