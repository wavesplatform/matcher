package com.wavesplatform.dex.grpc.integration.clients

import com.google.protobuf.empty.Empty
import com.wavesplatform.account.Address
import com.wavesplatform.dex.grpc.integration.clients.BalancesServiceClient.SpendableBalanceChanges
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

object BalancesServiceClient {
  type SpendableBalance        = Map[Asset, Long]
  type SpendableBalanceChanges = Map[Address, SpendableBalance]
}

class BalancesServiceClient(channel: Channel) extends ScorexLogging {

  private val balancesService                = BalancesServiceGrpc.stub(channel)
  private val spendableBalanceChangesSubject = ConcurrentSubject.publish[SpendableBalanceChanges]

  private def toVanilla(record: BalanceChangesResponse.Record): (Address, Asset, Long) = {
    (record.address.toVanillaAddress, record.asset.toVanillaAsset, record.balance)
  }

  /**
    * Grouping batch records of spendable balances changes by addresses, e.g. converts
    * {{{ Seq[(Address, Asset, Balance)] to Map[Address, Map[Asset, Balance]] }}}
    */
  private def groupByAddress(balanceChangesResponse: BalanceChangesResponse): SpendableBalanceChanges = {
    balanceChangesResponse.batch
      .map { toVanilla }
      .groupBy { case (address, _, _) => address }
      .mapValues { _.map { case (_, asset, balance) => asset -> balance }.toMap.withDefaultValue(0) }
  }

  private val balanceChangesObserver: StreamObserver[BalanceChangesResponse] =
    new StreamObserver[BalanceChangesResponse] {
      override def onError(t: Throwable): Unit                 = log.warn(s"Error while listening to the balance changes stream occured: ${t.getMessage}")
      override def onCompleted(): Unit                         = log.info("Balance changes stream completed!")
      override def onNext(value: BalanceChangesResponse): Unit = groupByAddress(value) |> spendableBalanceChangesSubject.onNext
    }

  /** Performs new gRPC call for receiving of the spendable balance changes stream */
  def requestBalanceChanges(): Unit = balancesService.getBalanceChanges(Empty(), balanceChangesObserver)

  /** Returns stream of the balance changes as a sequence of batches */
  def spendableBalanceChanges: Observable[SpendableBalanceChanges] = spendableBalanceChangesSubject
}
