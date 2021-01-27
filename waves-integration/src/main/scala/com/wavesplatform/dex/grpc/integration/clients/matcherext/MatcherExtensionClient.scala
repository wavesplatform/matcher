package com.wavesplatform.dex.grpc.integration.clients.matcherext

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.{BlockRef, BlockchainBalance, DiffIndex}
import com.wavesplatform.dex.grpc.integration.clients.{BroadcastResult, CheckedBroadcastResult, RunScriptResult}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription

import scala.concurrent.Future

trait MatcherExtensionClient {

  val utxEvents: UtxEventsControlledStream

  def getBalances(index: DiffIndex): Future[BlockchainBalance]

  def getOutgoingLeasing(address: Address): Future[Long]
  def getAddressPartialRegularBalance(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]]
  def getAddressFullRegularBalance(address: Address, excludeAssets: Set[Asset]): Future[Map[Asset, Long]]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): Future[BroadcastResult]
  def checkedBroadcastTx(tx: ExchangeTransaction): Future[CheckedBroadcastResult]

  def isOrderConfirmed(orderId: ByteStr): Future[Boolean]

  def currentBlockInfo: Future[BlockRef]

  def close(): Future[Unit]
}
