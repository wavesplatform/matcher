package com.wavesplatform.dex.grpc.integration.clients.sync

import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainClient.RunScriptResult
import com.wavesplatform.dex.grpc.integration.dto.BriefAssetDescription
import com.wavesplatform.dex.grpc.integration.protobuf.ToPbConversions._
import com.wavesplatform.dex.grpc.integration.protobuf.ToVanillaConversions._
import com.wavesplatform.dex.grpc.integration.services.RunScriptResponse.Result
import com.wavesplatform.dex.grpc.integration.services._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.{exchange => ve}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel

class WavesBlockchainGrpcSyncClient(channel: ManagedChannel) extends WavesBlockchainClient with ScorexLogging {

  private val blockchainService = WavesBlockchainApiGrpc.blockingStub(channel)

  override def wasForged(id: ByteStr): Boolean = {
    blockchainService
      .getStatuses { TransactionsByIdRequest(Seq(ByteString copyFrom id.arr)) }
      .transactionsStatutes
      .headOption
      .exists(_.status.isConfirmed)
  }

  override def broadcastTx(tx: ve.ExchangeTransaction): Boolean = {
    blockchainService.broadcast { BroadcastRequest(transaction = Some(tx.toPB)) }.isValid
  }

  override def isFeatureActivated(id: Short): Boolean = {
    blockchainService.isFeatureActivated { IsFeatureActivatedRequest(featureId = id) }.isActivated
  }

  override def assetDescription(asset: IssuedAsset): Option[BriefAssetDescription] = {
    blockchainService.assetDescription { AssetIdRequest(assetId = asset.id.toPB) }.maybeDescription.toVanilla
  }

  override def hasScript(asset: IssuedAsset): Boolean = {
    blockchainService.hasAssetScript { AssetIdRequest(assetId = asset.toPB) }.has
  }

  override def runScript(asset: IssuedAsset, input: ve.ExchangeTransaction): RunScriptResult = {
    parse {
      blockchainService.runAssetScript {
        RunAssetScriptRequest(assetId = asset.toPB, transaction = Some(input.toPB))
      }
    }
  }

  override def hasScript(address: Address): Boolean = {
    blockchainService.hasAddressScript { HasAddressScriptRequest(address = address.toPB) }.has
  }

  override def runScript(address: Address, input: ve.Order): RunScriptResult = {
    parse {
      blockchainService.runAddressScript {
        RunAddressScriptRequest(address = address.toPB, order = Some(input.toPB))
      }
    }
  }

  override def spendableBalance(address: Address, asset: Asset): Long = {
    blockchainService.spendableAssetBalance { SpendableAssetBalanceRequest(address = address.toPB, assetId = asset.toPB) }.balance
  }

  override def forgedOrder(orderId: ByteStr): Boolean = {
    blockchainService.forgedOrder { ForgedOrderRequest(orderId.toPB) }.isForged
  }

  private def parse(input: RunScriptResponse): RunScriptResult = input.result match {
    case Result.WrongInput(message)   => throw new IllegalArgumentException(message)
    case Result.Empty                 => RunScriptResult.Allowed
    case Result.ScriptError(message)  => RunScriptResult.ScriptError(message)
    case Result.UnexpectedResult(obj) => RunScriptResult.UnexpectedResult(obj)
    case Result.Exception(value)      => RunScriptResult.Exception(value.name, value.message)
    case _: Result.Denied             => RunScriptResult.Denied
  }
}
