package com.wavesplatform.dex.grpc.integration.clients

import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.protobuf.Implicits._
import com.wavesplatform.dex.grpc.integration.services.balances.{BalanceChangesResponse, BalancesServiceGrpc}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import io.grpc.Channel
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import mouse.any._

class BalanceServiceClient(channel: Channel) extends ScorexLogging {

  private val balancesService                = BalancesServiceGrpc.stub(channel)
  private val spendableBalanceChangesSubject = ConcurrentSubject.publish[(Address, Asset, Long)]

  private val balanceChangesObserver: StreamObserver[BalanceChangesResponse] =
    new StreamObserver[BalanceChangesResponse] {
      override def onError(t: Throwable): Unit = log.warn(s"Error while listening to balance changes stream occured: ${t.getMessage}")
      override def onCompleted(): Unit         = log.info("Balance changes stream completed!")
      override def onNext(value: BalanceChangesResponse): Unit = value.balanceChangesBatch.foreach { record =>
        (record.address.toVanillaAddress, record.getAsset.toVanillaAsset, record.balance) |> spendableBalanceChangesSubject.onNext
      }
    }

  def requestBalanceChanges(): Unit = balancesService.getBalanceChanges(Empty(), balanceChangesObserver)

  def spendableBalanceChanges: Observable[(Address, Asset, Long)] = spendableBalanceChangesSubject
}
