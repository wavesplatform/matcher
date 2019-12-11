package com.wavesplatform.dex.grpc.integration.clients

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.SpendableBalanceChanges
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import monix.reactive.Observable

object WavesBlockchainClient {
  type SpendableBalance        = Map[Asset, Long]
  type SpendableBalanceChanges = Map[Address, SpendableBalance]
}

trait WavesBlockchainClient[F[_]] {
  def spendableBalanceChanges: Observable[SpendableBalanceChanges]
  def spendableBalance(address: Address, asset: Asset): F[Long]

  def isFeatureActivated(id: Short): F[Boolean]

  def assetDescription(asset: IssuedAsset): F[Option[BriefAssetDescription]]

  def hasScript(asset: IssuedAsset): F[Boolean]
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): F[RunScriptResult]

  def hasScript(address: Address): F[Boolean]
  def runScript(address: Address, input: Order): F[RunScriptResult]

  def wasForged(txIds: Seq[ByteStr]): F[Map[ByteStr, Boolean]]
  def broadcastTx(tx: ExchangeTransaction): F[Boolean]

  def forgedOrder(orderId: ByteStr): F[Boolean]
}
