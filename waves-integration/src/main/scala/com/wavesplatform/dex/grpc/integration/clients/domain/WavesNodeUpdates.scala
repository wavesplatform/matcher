package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import cats.instances.map._
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction

case class WavesNodeUpdates(
                             balanceUpdates: Map[Address, AddressBalanceUpdates],
                             unconfirmedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
                             confirmedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
                             failedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges]
) {
  def isEmpty: Boolean = balanceUpdates.isEmpty && unconfirmedTxs.isEmpty && confirmedTxs.isEmpty && failedTxs.isEmpty
}

object WavesNodeUpdates {

  implicit val updatesMonoid: Monoid[WavesNodeUpdates] = new Monoid[WavesNodeUpdates] {
    override val empty: WavesNodeUpdates = WavesNodeUpdates(Map.empty, Map.empty, Map.empty, Map.empty)

    override def combine(x: WavesNodeUpdates, y: WavesNodeUpdates): WavesNodeUpdates = WavesNodeUpdates(
      balanceUpdates = x.balanceUpdates |+| y.balanceUpdates,
      unconfirmedTxs = x.unconfirmedTxs ++ y.unconfirmedTxs,
      confirmedTxs = x.confirmedTxs ++ y.confirmedTxs,
      failedTxs = x.failedTxs ++ y.failedTxs
    )

  }

}
