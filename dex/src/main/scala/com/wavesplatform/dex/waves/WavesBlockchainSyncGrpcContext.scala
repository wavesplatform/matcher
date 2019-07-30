package com.wavesplatform.dex.waves

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.api.grpc.RunScriptResponse.Result
import com.wavesplatform.dex.api.grpc.ToPbConversions._
import com.wavesplatform.dex.api.grpc.ToVanillaConversions._
import com.wavesplatform.dex.api.grpc.{TransactionsByIdRequest, WavesBlockchainApiGrpc, _}
import com.wavesplatform.dex.model.BriefAssetDescription
import com.wavesplatform.dex.waves.WavesBlockchainContext.RunScriptResult
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import io.grpc.ManagedChannel
import monix.reactive.Observable

class WavesBlockchainSyncGrpcContext(channel: ManagedChannel) extends WavesBlockchainContext {
  private val waves = WavesBlockchainApiGrpc.blockingStub(channel)

  override def wasForged(id: ByteStr): Boolean =
    waves
      .getStatuses(TransactionsByIdRequest(Seq(ByteString.copyFrom(id.arr))))
      .toIterable
      .headOption
      .exists(_.status.isConfirmed)

  override def broadcastTx(tx: exchange.ExchangeTransaction): Boolean = waves.broadcast(BroadcastRequest(transaction = Some(tx.toPB))).isValid

  override def isFeatureActivated(id: Short): Boolean =
    waves.isFeatureActivated(IsFeatureActivatedRequest(featureId = id)).isActivated

  override def assetDescription(asset: IssuedAsset): Option[BriefAssetDescription] =
    waves.assetDescription(AssetIdRequest(assetId = asset.id.toPB)).maybeDescription.toVanilla

  override def hasScript(asset: IssuedAsset): Boolean = waves.hasAssetScript(AssetIdRequest(assetId = asset.id.toPB)).has

  override def runScript(asset: IssuedAsset, input: ExchangeTransaction): RunScriptResult =
    parse(waves.runAssetScript(RunAssetScriptRequest(assetId = asset.id.toPB, transaction = Some(input.toPB))))

  override def hasScript(address: Address): Boolean = waves.hasAddressScript(HasAddressScriptRequest(address = address.bytes.toPB)).has

  override def runScript(address: Address, input: Order): RunScriptResult =
    parse(
      waves.runAddressScript(
        RunAddressScriptRequest(
          address = address.bytes.toPB,
          order = Some(input.toPB)
        )))

  override def spendableBalanceChanged: Observable[(Address, Asset)] = Observable.empty

  override def spendableBalance(address: Address, asset: Asset): Long =
    waves
      .spendableAssetBalance(
        SpendableAssetBalanceRequest(
          address = address.bytes.toPB,
          assetId = Some(asset.toPB)
        )
      )
      .balance

  override def forgedOrder(orderId: ByteStr): Boolean = waves.forgedOrder(ForgedOrderRequest(orderId.toPB)).isForged

  private def parse(input: RunScriptResponse): RunScriptResult = input.result match {
    case Result.WrongInput(message)   => throw new IllegalArgumentException(message)
    case Result.Empty                 => RunScriptResult.Allowed
    case Result.ScriptError(message)  => RunScriptResult.ScriptError(message)
    case Result.UnexpectedResult(obj) => RunScriptResult.UnexpectedResult(obj)
    case Result.Exception(value)      => RunScriptResult.Exception(value.name, value.message)
    case _: Result.Denied             => RunScriptResult.Denied
  }
}
