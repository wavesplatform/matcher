package com.wavesplatform.dex.grpc.integration.clients

import java.net.InetAddress

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.reactive.Observable

import scala.concurrent.{ExecutionContext, Future}

object WavesBlockchainClient {

  case class Updates(updatedBalances: Map[Address, Map[Asset, Long]])

}

trait WavesBlockchainClient {
  def updates: Observable[WavesBlockchainClient.Updates]

  def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]]

  def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): Future[Boolean]

  def forgedOrder(orderId: ByteStr): Future[Boolean]

  def getNodeAddress: Future[InetAddress]

  def close(): Future[Unit]
}

class DefaultWavesBlockchainClient(
  meClient: MatcherExtensionClient[Future],
  bClient: BlockchainUpdatesClient
)(implicit ec: ExecutionContext) extends WavesBlockchainClient {

  override def updates: Observable[Updates] = {
    val meStream = meClient.realTimeBalanceChanges.map(x => Map(x.address -> Map(x.asset -> x.balance)))
    val bStream = bClient.blockchainUpdates
    Observable(bStream, meStream).merge.map(Updates)
  }

  override def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]] =
    meClient.spendableBalances(address, assets)

  override def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]] =
    meClient.allAssetsSpendableBalance(address)

  // TODO get all and track in bClient
  override def isFeatureActivated(id: Short): Future[Boolean] =
    meClient.isFeatureActivated(id)

  // TODO track?
  override def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]] =
    meClient.assetDescription(asset)

  // TODO track?
  override def hasScript(asset: IssuedAsset): Future[Boolean] = meClient.hasScript(asset)

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult] =
    meClient.runScript(asset, input)

  override def hasScript(address: Address): Future[Boolean] =
    meClient.hasScript(address)

  override def runScript(address: Address, input: Order): Future[RunScriptResult] =
    meClient.runScript(address, input)

  override def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]] =
    meClient.wereForged(txIds)

  override def broadcastTx(tx: ExchangeTransaction): Future[Boolean] =
    meClient.broadcastTx(tx)

  override def forgedOrder(orderId: ByteStr): Future[Boolean] =
    meClient.forgedOrder(orderId)

  override def getNodeAddress: Future[InetAddress] =
    meClient.getNodeAddress

  override def close(): Future[Unit] = meClient.close().zip(bClient.close()).map(_ => ())
}
