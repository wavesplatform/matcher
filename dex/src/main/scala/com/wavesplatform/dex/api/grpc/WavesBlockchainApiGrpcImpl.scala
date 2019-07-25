package com.wavesplatform.dex.api.grpc

import com.wavesplatform.protobuf.transaction.{PBSignedTransaction, VanillaTransaction}
import com.wavesplatform.state.Blockchain
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

  override def broadcast(tx: PBSignedTransaction): Future[BroadcastResponse] = {
    val vanillaTx = tx.toVanilla
    utx
      .putIfNew(vanillaTx)
      .map { _ =>
        broadcastTransaction(vanillaTx)
        BroadcastResponse.defaultInstance
      }
      .resultE
      .toFuture
  }

  override def isFeatureActivated(request: IsFeatureActivatedRequest): Future[IsFeatureActivatedResponse] = ???

  override def assetDescription(request: AssetIdRequest): Future[AssetDescriptionResponse] = ???

  override def hasScript(request: AssetIdRequest): Future[HasScriptResponse] = ???

  override def runAssetScript(request: RunAssetScriptRequest): Future[RunAssetScriptResponse] = ???
}
