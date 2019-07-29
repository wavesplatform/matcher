package com.wavesplatform.dex.waves

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.api.grpc.{
  BroadcastRequest,
  SignedExchangeTransaction,
  TransactionsByIdRequest,
  VanillaTransactionConversions,
  WavesBlockchainApiGrpc
}
import com.wavesplatform.dex.waves.WavesBlockchainContext.RunScriptResult
import com.wavesplatform.state.AssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import io.grpc.{ManagedChannel, ManagedChannelBuilder}
import monix.reactive.Observable

trait WavesBlockchainContext {
  // TODO multiple ids
  def wasForged(id: ByteStr): Boolean
  def broadcastTx(txs: ExchangeTransaction): Boolean

  def isFeatureActivated(id: Short): Boolean

  def assetDescription(asset: IssuedAsset): Option[AssetDescription]

  def hasScript(asset: IssuedAsset): Boolean
  def runScript(asset: IssuedAsset, input: ExchangeTransaction): RunScriptResult

  def hasScript(address: Address): Boolean
  def runScript(address: Address, input: Order): RunScriptResult

  def spendableBalanceChanged: Observable[(Address, Asset)]
  def spendableBalance(address: Address, asset: Asset): Long

  def forgedOrder(orderId: ByteStr): Boolean
}

object WavesBlockchainContext {
  sealed trait RunScriptResult
  object RunScriptResult {
    case class ScriptError(message: String)             extends RunScriptResult
    case object Denied                                  extends RunScriptResult
    case object Allowed                                 extends RunScriptResult
    case class UnexpectedResult(rawResult: String)      extends RunScriptResult
    case class Exception(name: String, message: String) extends RunScriptResult
  }
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

  override def broadcastTx(tx: exchange.ExchangeTransaction): Boolean =
    waves
      .broadcast(
        BroadcastRequest(
          transaction = Some(
            SignedExchangeTransaction(
              transaction = Some(tx.toPB),
              proofs = tx.proofs.proofs.map(bytes => ByteString.copyFrom(bytes.arr))
            ))))
      .isValid

  override def isFeatureActivated(id: Short): Boolean                                     = ???
  override def assetDescription(asset: IssuedAsset): Option[AssetDescription]             = ???
  override def hasScript(asset: IssuedAsset): Boolean                                     = ???
  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): RunScriptResult = ???
  override def hasScript(address: Address): Boolean                                       = ???
  override def runScript(address: Address, input: Order): RunScriptResult                 = ???
  override def spendableBalanceChanged: Observable[(Address, Asset)]                      = ???
  override def spendableBalance(address: Address, asset: Asset): Long                     = ???
  override def forgedOrder(orderId: ByteStr): Boolean                                     = ???
}

object WavesBlockchainGrpcContext {
  def apply(matcherAddress: Address, host: String, port: Int): WavesBlockchainContext =
    new WavesBlockchainGrpcContext(matcherAddress, ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build())
}
