package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import cats.instances.list._
import cats.instances.map._
import cats.instances.set._
import cats.syntax.foldable._
import cats.syntax.monoid._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates.addressTxOf
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.protobuf.transaction.Transaction

/**
 * CombinedWavesBlockchainClient guarantees, that all transactions are ExchangeTransactions
 */
case class WavesNodeUpdates(
  balanceUpdates: Map[Address, AddressBalanceUpdates],
  unconfirmedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
  confirmedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges],
  failedTxs: Map[ExchangeTransaction.Id, TransactionWithChanges]
) {
  def isEmpty: Boolean = balanceUpdates.isEmpty && unconfirmedTxs.isEmpty && confirmedTxs.isEmpty && failedTxs.isEmpty
  def observedTxIdsByAddresses: Map[Address, Set[ExchangeTransaction.Id]] = List(unconfirmedTxs, confirmedTxs, failedTxs).foldMap(addressTxOf)

  def updatesByAddresses: Map[Address, (AddressBalanceUpdates, Set[ExchangeTransaction.Id])] = {
    val observedTxIds = observedTxIdsByAddresses
    (balanceUpdates.keySet ++ observedTxIds.keySet).view
      .map { address =>
        (
          address,
          (
            balanceUpdates.getOrElse(address, AddressBalanceUpdates.empty),
            observedTxIds.getOrElse(address, Set.empty)
          )
        )
      }
      .toMap
  }

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

  def addressTxOf(xs: Map[ExchangeTransaction.Id, TransactionWithChanges]): Map[Address, Set[ExchangeTransaction.Id]] =
    xs.foldLeft(Map.empty[Address, Set[ExchangeTransaction.Id]].withDefaultValue(Set.empty)) {
      case (r, (txId, tx)) =>
        val addresses = for {
          tx <- tx.tx.transaction.toSet[Transaction] // maker and taker could be the same
          order <- tx.getExchange.orders
        } yield order.senderPublicKey.toVanillaPublicKey.toAddress

        addresses.foldLeft(r) {
          case (r, address) => r.updated(address, r(address) + txId)
        }
    }

}
