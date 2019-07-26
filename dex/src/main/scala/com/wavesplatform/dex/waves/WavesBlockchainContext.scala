package com.wavesplatform.dex.waves

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.api.grpc.VanillaTransactionConversions
import com.wavesplatform.dex.api.grpc.{BroadcastRequest, TransactionsByIdRequest, WavesBlockchainApiGrpc}
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.AssetDescription
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, Transaction}
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.reactive.Observable

trait WavesBlockchainContext {
  // TODO multiple ids
  def wasForged(id: ByteStr): Boolean
  def broadcastTx(txs: Transaction): Boolean

  def isFeatureActivated(id: Short): Boolean

  def assetDescription(asset: IssuedAsset): Option[AssetDescription]

  def hasScript(asset: IssuedAsset): Boolean
  def runScript(asset: IssuedAsset, input: Transaction): Either[String, Terms.EVALUATED]

  def hasScript(address: Address): Boolean
  def runScript(address: Address, input: Order): Either[String, Terms.EVALUATED]

  def spendableBalanceChanged: Observable[(Address, Asset)]
  def spendableBalance(address: Address, asset: Asset): Long

  def forgedOrder(orderId: ByteStr): Boolean
}

class WavesBlockchainGrpcContext(matcherAddress: Address, channel: ManagedChannel) extends WavesBlockchainContext {
  private val waves = WavesBlockchainApiGrpc.blockingStub(channel)

  // sender is mandatory!
  override def wasForged(id: ByteStr): Boolean =
    waves
      .getStatuses(TransactionsByIdRequest(Seq(ByteString.copyFrom(id.arr))))
      .toIterable
      .headOption
      .exists(_.status.isConfirmed)

  override def broadcastTx(tx: Transaction): Boolean = waves.broadcast(BroadcastRequest(transaction = Some(tx.toPB))).isValid

  override def isFeatureActivated(id: Short): Boolean                                             = ???
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription]                     = ???
  override def hasScript(asset: IssuedAsset): Boolean                                             = ???
  override def runScript(asset: IssuedAsset, input: Transaction): Either[String, Terms.EVALUATED] = ???
  override def hasScript(address: Address): Boolean                                               = ???
  override def runScript(address: Address, input: Order): Either[String, Terms.EVALUATED]         = ???
  override def spendableBalanceChanged: Observable[(Address, Asset)]                              = ???
  override def spendableBalance(address: Address, asset: Asset): Long                             = ???
  override def forgedOrder(orderId: ByteStr): Boolean                                             = ???
}

object WavesBlockchainGrpcContext {
  def apply(matcherAddress: Address, host: String, port: Int): WavesBlockchainContext =
    new WavesBlockchainGrpcContext(matcherAddress, ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build())
}
