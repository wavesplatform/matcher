package com.wavesplatform.dex.grpc.integration.client

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.client.WavesBlockchainContext.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.ToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.ToVanillaConversions._
import com.wavesplatform.dex.grpc.integration.services.RunScriptResponse.Result
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.{exchange => ve}
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

  override def broadcastTx(tx: ve.ExchangeTransaction): Boolean = waves.broadcast(BroadcastRequest(transaction = Some(tx.toPB))).isValid

  override def isFeatureActivated(id: Short): Boolean =
    waves.isFeatureActivated(IsFeatureActivatedRequest(featureId = id)).isActivated

  override def assetDescription(asset: IssuedAsset): Option[BriefAssetDescription] =
    waves.assetDescription(AssetIdRequest(assetId = asset.id.toPB)).maybeDescription.toVanilla

  override def hasScript(asset: IssuedAsset): Boolean = waves.hasAssetScript(AssetIdRequest(assetId = asset.toPB)).has

  override def runScript(asset: IssuedAsset, input: ve.ExchangeTransaction): RunScriptResult =
    parse(waves.runAssetScript(RunAssetScriptRequest(assetId = asset.id.toPB, transaction = Some(input.toPB))))

  override def hasScript(address: Address): Boolean = waves.hasAddressScript(HasAddressScriptRequest(address = address.toPB)).has

  override def runScript(address: Address, input: ve.Order): RunScriptResult =
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
          assetId = asset.toPB
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
