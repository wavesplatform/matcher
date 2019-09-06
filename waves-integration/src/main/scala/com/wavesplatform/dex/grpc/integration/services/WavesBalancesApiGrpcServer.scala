package com.wavesplatform.dex.grpc.integration.services

import com.google.protobuf.empty.Empty
import com.wavesplatform.extensions.{Context => ExtensionContext}
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import mouse.any._
import com.wavesplatform.dex.grpc.integration.protobuf.ToPbConversions._

import scala.concurrent.duration.FiniteDuration

class WavesBalancesApiGrpcServer(context: ExtensionContext, balanceChangesBatchLingerMs: FiniteDuration)(implicit sc: Scheduler)
    extends WavesBalancesApiGrpc.WavesBalancesApi {

  /**
    * Pushes batches of the balance changes into provided `responseObserver`.
    * Batch period is equal to `balanceChangesBatchLingerMs` parameter.
    * On the server side the batch is Seq[(Address, Asset, Balance)]
    */
  override def getBalanceChanges(request: Empty, responseObserver: StreamObserver[BalanceChangesResponse]): Unit = {
    context.spendableBalanceChanged.bufferTimed(balanceChangesBatchLingerMs).foreach { changesBuffer =>
      changesBuffer.distinct.map { case (address, asset) => (address, asset, context.utx.spendableBalance(address, asset)) } |> { vanillaBatch =>
        vanillaBatch.map {
          case (address, asset, balance) => BalanceChangesResponse.Record(address.toPB, asset.toPB, balance)
        } |> BalanceChangesResponse.apply |> responseObserver.onNext
      }
    }
  }
}
