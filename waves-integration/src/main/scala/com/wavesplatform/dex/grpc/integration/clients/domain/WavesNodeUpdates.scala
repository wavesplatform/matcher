package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import cats.instances.map._
import cats.syntax.monoid._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates.addressTxOf
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.Implicits._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._

/**
 * CombinedWavesBlockchainClient guarantees, that all transactions are ExchangeTransactions
 */
case class WavesNodeUpdates(
  balanceUpdates: Map[Address, AddressBalanceUpdates],
  observedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges]
) {
  def isEmpty: Boolean = balanceUpdates.isEmpty && observedTxs.isEmpty
  def observedTxIdsByAddresses: Map[Address, Map[ExchangeTransaction.Id, Map[Asset, Long]]] = addressTxOf(observedTxs)

  def updatesByAddresses: Map[Address, (AddressBalanceUpdates, Map[ExchangeTransaction.Id, Map[Asset, Long]])] = {
    val observedTxIds = observedTxIdsByAddresses
    (balanceUpdates.keySet ++ observedTxIds.keySet).view
      .map { address =>
        (
          address,
          (
            balanceUpdates.getOrElse(address, AddressBalanceUpdates.empty),
            observedTxIds.getOrElse(address, Map.empty)
          )
        )
      }
      .toMap
  }

}

object WavesNodeUpdates {

  implicit val updatesMonoid: Monoid[WavesNodeUpdates] = new Monoid[WavesNodeUpdates] {
    override val empty: WavesNodeUpdates = WavesNodeUpdates(Map.empty, Map.empty)

    override def combine(x: WavesNodeUpdates, y: WavesNodeUpdates): WavesNodeUpdates = WavesNodeUpdates(
      balanceUpdates = x.balanceUpdates |+| y.balanceUpdates,
      observedTxs = x.observedTxs ++ y.observedTxs
    )

  }

  def addressTxOf(xs: Map[ExchangeTransaction.Id, TransactionWithChanges]): Map[Address, Map[ExchangeTransaction.Id, Map[Asset, Long]]] =
    xs.foldLeft(Map.empty[Address, Map[ExchangeTransaction.Id, Map[Asset, Long]]].withDefaultValue(Map.empty)) {
      case (r, (txId, tx)) =>
        val pp = tx.pessimisticPortfolios

        val addresses = for {
          tx <- tx.tx.transaction.toSeq
          order <- tx.getExchange.orders
        } yield order.senderPublicKey.toVanillaPublicKey.toAddress

        addresses.toSet // maker and taker could be the same
          .foldLeft(r) { case (r, address) => r.updated(address, r(address).updated(txId, pp.getOrElse(address, Map.empty))) }
    }

}
