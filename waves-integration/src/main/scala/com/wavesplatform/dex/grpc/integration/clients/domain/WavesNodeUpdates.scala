package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.collections.MapOps.Ops2D
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction

case class WavesNodeUpdates(
  updatedBalances: Map[Address, Map[Asset, Long]],
  appearedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
  failedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges]
) {
  def isEmpty: Boolean = updatedBalances.isEmpty && appearedTxs.isEmpty
}

object WavesNodeUpdates {

  implicit val updatesMonoid: Monoid[WavesNodeUpdates] = new Monoid[WavesNodeUpdates] {
    override val empty: WavesNodeUpdates = WavesNodeUpdates(Map.empty, Map.empty, Map.empty)

    override def combine(x: WavesNodeUpdates, y: WavesNodeUpdates): WavesNodeUpdates = WavesNodeUpdates(
      updatedBalances = x.updatedBalances.deepReplace(y.updatedBalances),
      appearedTxs = x.appearedTxs ++ y.appearedTxs,
      failedTxs = x.failedTxs ++ y.failedTxs
    )

  }

}
