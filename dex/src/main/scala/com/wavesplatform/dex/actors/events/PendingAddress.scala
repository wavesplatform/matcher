package com.wavesplatform.dex.actors.events

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.model.Events

import scala.collection.immutable.Queue

case class PendingAddress(
  pendingTxs: Map[ExchangeTransaction.Id, PendingTransactionType],
  stashedBalance: Map[Asset, Long],
  events: Queue[Events.Event]
) {
  def isResolved: Boolean = pendingTxs.isEmpty

  def withUpdatedBalances(xs: Map[Asset, Long]): PendingAddress = copy(stashedBalance = stashedBalance ++ xs)

  def withKnownOnNode(txId: ExchangeTransaction.Id, balanceUpdates: Map[Asset, Long]): PendingAddress = {
    val updatedStashedBalance = stashedBalance ++ balanceUpdates
    pendingTxs.get(txId) match {
      case Some(PendingTransactionType.KnownOnNode) => copy(stashedBalance = updatedStashedBalance)
      case Some(PendingTransactionType.KnownOnMatcher) =>
        copy(
          pendingTxs = pendingTxs - txId,
          stashedBalance = updatedStashedBalance
        )
      case _ =>
        copy(
          pendingTxs = pendingTxs.updated(txId, PendingTransactionType.KnownOnNode),
          stashedBalance = updatedStashedBalance
        )
    }
  }

  def withKnownOnMatcher(txId: ExchangeTransaction.Id, event: Events.OrderExecuted): PendingAddress =
    pendingTxs.get(txId) match {
      case Some(PendingTransactionType.KnownOnMatcher) => this
      case Some(PendingTransactionType.KnownOnNode) =>
        copy(
          pendingTxs = pendingTxs - txId,
          events = events.enqueue(event)
        )
      case _ =>
        copy(
          pendingTxs = pendingTxs.updated(txId, PendingTransactionType.KnownOnMatcher),
          events = events.enqueue(event)
        )
    }

  def withEvent(event: Events.OrderCanceled): PendingAddress = copy(events = events.enqueue(event))
}
