package com.wavesplatform.dex.grpc.integration.clients

import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream
import com.wavesplatform.dex.grpc.integration.clients.domain.{AddressBalanceUpdates, WavesNodeUpdates}
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import monix.reactive.Observable

import scala.concurrent.Future

// TODO DEX-998
trait WavesBlockchainClient {

  /**
   * @return (update, ready) ready == the last processed height >= the last height in blockchain
   */
  def updates: Observable[(WavesNodeUpdates, Boolean)]

  def partialBalancesSnapshot(address: Address, assets: Set[Asset]): Future[AddressBalanceUpdates]
  def fullBalancesSnapshot(address: Address, excludeAssets: Set[Asset]): Future[AddressBalanceUpdates]

  def isFeatureActivated(id: Short): Future[Boolean]

  def assetDescription(asset: IssuedAsset): Future[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): Future[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): Future[RunScriptResult]

  def hasScript(address: Address): Future[Boolean]
  def runScript(address: Address, input: Order): Future[RunScriptResult]

  /**
   * Confirmed or not
   */
  def areKnown(txIds: Seq[ByteStr]): Future[Map[ByteStr, Boolean]]

  // TODO Deprecated, remove in >= 2.3.1
  def broadcastTx(tx: ExchangeTransaction): Future[BroadcastResult]

  def checkedBroadcastTx(tx: ExchangeTransaction): Future[CheckedBroadcastResult]

  def isOrderConfirmed(orderId: ByteStr): Future[Boolean]

  def close(): Future[Unit]

  def status(): CombinedStream.Status
}
