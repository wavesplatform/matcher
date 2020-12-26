package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.collections.MapOps.Ops2D
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction

case class WavesNodeUpdates(
  updatedBalances: Map[Address, Map[Asset, Long]],
  unconfirmedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
  confirmedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
  failedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges]
) {
  def isEmpty: Boolean = updatedBalances.isEmpty && unconfirmedTxs.isEmpty && confirmedTxs.isEmpty && failedTxs.isEmpty
}

object WavesNodeUpdates {

  implicit val updatesMonoid: Monoid[WavesNodeUpdates] = new Monoid[WavesNodeUpdates] {
    override val empty: WavesNodeUpdates = WavesNodeUpdates(Map.empty, Map.empty, Map.empty, Map.empty)

    override def combine(x: WavesNodeUpdates, y: WavesNodeUpdates): WavesNodeUpdates = WavesNodeUpdates(
      updatedBalances = x.updatedBalances.deepReplace(y.updatedBalances),
      unconfirmedTxs = x.unconfirmedTxs ++ y.unconfirmedTxs,
      confirmedTxs = x.confirmedTxs ++ y.confirmedTxs,
      failedTxs = x.failedTxs ++ y.failedTxs
    )

  }

}
