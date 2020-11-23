package com.wavesplatform.dex.grpc.integration.clients.matcherext

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.RunScriptResult
import com.wavesplatform.dex.grpc.integration.clients.status.{BlockRef, BlockchainBalance, DiffIndex, WavesNodeEvent}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.reactive.Observable

import scala.concurrent.Future

trait MatcherExtensionClient {

  def utxEvents: Observable[WavesNodeEvent]

  def spendableBalances(address: Address, assets: Set[Asset]): Future[Map[Asset, Long]]
  def allAssetsSpendableBalance(address: Address): Future[Map[Asset, Long]]
  def getBalances(index: DiffIndex): Future[BlockchainBalance]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  def wereForged(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): Future[Boolean]

  def forgedOrder(orderId: ByteStr): Future[Boolean]

  def currentBlockInfo: Future[BlockRef]

  def close(): Future[Unit]
}
