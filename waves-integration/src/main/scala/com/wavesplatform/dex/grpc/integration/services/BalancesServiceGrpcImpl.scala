package com.wavesplatform.dex.grpc.integration.services

import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.grpc.integration.protobuf.Implicits._
import com.wavesplatform.dex.grpc.integration.services.balances._
import com.wavesplatform.extensions.{Context => ExtensionContext}
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import mouse.any._

import scala.concurrent.duration.FiniteDuration

class BalancesServiceGrpcImpl(context: ExtensionContext, balanceChangesBatchLingerMs: FiniteDuration)(implicit sc: Scheduler)
    extends BalancesServiceGrpc.BalancesService {

  override def getBalanceChanges(request: Empty, responseObserver: StreamObserver[BalanceChangesResponse]): Unit = {
    context.spendableBalanceChanged.bufferTimed(balanceChangesBatchLingerMs).foreach { batchedChanges =>
      batchedChanges.map { case (address, asset) => (address, asset, context.blockchain.balance(address, asset)) }.distinct |> { vanillaBatch =>
        vanillaBatch.map {
          case (address, asset, balance) => BalanceChangesResponse.Record(address.toPBAddress, asset.toPBAsset, balance)
        } |> BalanceChangesResponse.apply |> responseObserver.onNext
      }
    }
  }
}
