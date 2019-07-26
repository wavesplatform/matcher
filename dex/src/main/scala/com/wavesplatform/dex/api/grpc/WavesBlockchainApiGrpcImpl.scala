package com.wavesplatform.dex.api.grpc

import cats.syntax.either._
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction.VanillaTransaction
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utx.UtxPool
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Future

class WavesBlockchainApiGrpcImpl(blockchain: Blockchain, utx: UtxPool, broadcastTransaction: VanillaTransaction => Unit)(implicit sc: Scheduler)
    extends WavesBlockchainApiGrpc.WavesBlockchainApi {

  override def getStatuses(request: TransactionsByIdRequest, responseObserver: StreamObserver[TransactionStatus]): Unit = {
    val result = Observable(request.transactionIds: _*).map { txId =>
      blockchain.transactionHeight(to(txId)) match {
        case Some(height) => TransactionStatus(txId, TransactionStatus.Status.CONFIRMED, height) // TODO
        case None =>
          utx.transactionById(to(txId)) match {
            case Some(_) => TransactionStatus(txId, TransactionStatus.Status.UNCONFIRMED)
            case None    => TransactionStatus(txId, TransactionStatus.Status.NOT_EXISTS)
          }
      }
    }
    responseObserver.completeWith(result)
  }

  override def broadcast(request: BroadcastRequest): Future[BroadcastResponse] =
    request.transaction
      .map(_.toVanilla)
      .fold[Either[ValidationError, BroadcastResponse]](GenericError("Expected a transaction").asLeft) { tx =>
        utx
          .putIfNew(tx)
          .map { _ =>
            broadcastTransaction(tx)
            BroadcastResponse(isValid = true)
          }
          .resultE
          .leftFlatMap(_ => BroadcastResponse().asRight)
      }
      .toFuture

  override def isFeatureActivated(request: IsFeatureActivatedRequest): Future[IsFeatureActivatedResponse] = Future {
    IsFeatureActivatedResponse(isActivated = blockchain.activatedFeatures.contains(request.featureId.toShort))
  }

  override def assetDescription(request: AssetIdRequest): Future[AssetDescriptionResponse] = Future {
    import AssetDescriptionResponse._

    val desc = blockchain.assetDescription(IssuedAsset(request.assetId.toByteArray))
    val gRpcDesc = desc.fold[MaybeDescription](MaybeDescription.Empty) { desc =>
      MaybeDescription.Description(
        AssetDescription(
          name = ByteString.copyFrom(desc.name),
          decimals = desc.decimals,
          hasScript = desc.script.nonEmpty
        )
      )
    }

    AssetDescriptionResponse(gRpcDesc)
  }

  override def hasAssetScript(request: AssetIdRequest): Future[HasScriptResponse] = Future {
    HasScriptResponse(has = blockchain.hasAssetScript(IssuedAsset(to(request.assetId))))
  }

  override def runAssetScript(request: RunAssetScriptRequest): Future[RunScriptResponse] = ???

  override def hasAddressScript(request: HasAddressScriptRequest): Future[HasScriptResponse] =
    Address.fromBytes(to(request.address)).toFuture.map { addr =>
      HasScriptResponse(has = blockchain.hasScript(addr))
    }

  override def runAddressScript(request: RunAddressScriptRequest): Future[RunScriptResponse] = ???

  override def spendableAssetBalance(request: SpendableAssetBalanceRequest): Future[SpendableAssetBalanceResponse] =
    for {
      addr    <- Address.fromBytes(to(request.address)).toFuture
      assetId <- Future { request.assetId.fold[Asset](Waves)(issued => IssuedAsset(to(issued.getIssuedAsset))) }
    } yield SpendableAssetBalanceResponse(blockchain.balance(addr, assetId))

  override def forgedOrder(request: ForgedOrderRequest): Future[ForgedOrderResponse] = Future {
    val seen = blockchain.filledVolumeAndFee(to(request.orderId)).volume > 0
    ForgedOrderResponse(isForged = seen)
  }
}
