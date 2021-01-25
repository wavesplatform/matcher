package com.wavesplatform.dex.actors.events

import cats.syntax.option._
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.model.Events

import scala.collection.immutable.Queue

/**
 * Holds changes (executed and cancel events and balance changes) until OrderExecuted is resolved by a transaction.
 * It could be resolved in two ways:
 * 1. Direct order:
 *   1. We create and broadcast a transaction (withExecuted)
 *   2. We hold changes for maker and taker
 *   3. We receive the sent transaction from the blockchain stream (withKnownOnNodeTx, with same id)
 *   4. Changes are passed
 * 2. Indirect order:
 *   1. We receive a transaction, which we haven't yet created (withKnownOnNodeTx)
 *   2. We hold changes for maker and taker
 *   3. We create the received transaction (withExecuted, with same id)
 *   4. Changes are passed
 */
case class OrderEventsCoordinatorActorState(addresses: Map[Address, PendingAddress], knownOnNodeCache: FifoSet[ExchangeTransaction.Id]) {

  /**
   * @return (updated, passUpdates), passUpdates can be sent to recipients
   */
  def withBalanceUpdates(updates: AddressAssets): (OrderEventsCoordinatorActorState, AddressAssets) = {
    val (updatedAddresses, passUpdates) = updates.foldLeft((addresses, Map.empty: AddressAssets)) {
      case ((addresses, passUpdates), item @ (address, updates)) =>
        addresses.get(address) match {
          case Some(x) => (addresses.updated(address, x.withUpdatedBalances(updates)), passUpdates)
          case None =>
            if (updates.isEmpty) (addresses, passUpdates)
            else (addresses, passUpdates + item)
        }
    }

    (OrderEventsCoordinatorActorState(updatedAddresses, knownOnNodeCache), passUpdates)
  }

  /**
   * @return (updated, passEvent), passEvent can be sent to a recipient
   */
  def withPendingCancel(event: Events.OrderCanceled): (OrderEventsCoordinatorActorState, Option[Events.OrderCanceled]) = {
    val address = event.acceptedOrder.order.senderPublicKey.toAddress
    addresses.get(address) match {
      case Some(x) => (OrderEventsCoordinatorActorState(addresses.updated(address, x.withEvent(event)), knownOnNodeCache), none)
      case _ => (this, event.some)
    }
  }

  /**
   * @return (updated, resolved)
   */
  def withKnownOnMatcher(
    txId: ExchangeTransaction.Id,
    event: Events.OrderExecuted
  ): (OrderEventsCoordinatorActorState, Map[Address, PendingAddress]) = {
    lazy val defaultPendingAddress = PendingAddress(
      pendingTxs = Map[ExchangeTransaction.Id, PendingTransactionType](txId -> PendingTransactionType.KnownOnMatcher),
      stashedBalance = Map.empty,
      events = Queue(event)
    )

    val (updatedAddresses, resolved) = event.traders.foldLeft((addresses, Map.empty[Address, PendingAddress])) {
      case ((addresses, resolved), address) =>
        val x = addresses.get(address).fold(defaultPendingAddress)(_.withKnownOnMatcher(txId, event))
        if (x.isResolved) (addresses - address, resolved.updated(address, x))
        else (addresses.updated(address, x), resolved)
    }

    (OrderEventsCoordinatorActorState(updatedAddresses, knownOnNodeCache), resolved)
  }

  /**
   * @return (updated, passUpdates, resolved)
   * @note balanceUpdates probably should be separated from withKnownOnNode
   */
  def withKnownOnNode(
    traders: Set[Address],
    txId: ExchangeTransaction.Id,
    balanceUpdates: AddressAssets
  ): (OrderEventsCoordinatorActorState, AddressAssets, Map[Address, PendingAddress]) =
    if (knownOnNodeCache.contains(txId)) {
      val (updated, restUpdates) = withBalanceUpdates(balanceUpdates)
      (updated, restUpdates, Map.empty)
    } else {
      val (updatedAddresses1, restUpdates1, resolved) = traders.foldLeft((addresses, balanceUpdates, Map.empty[Address, PendingAddress])) {
        case ((addresses, restUpdates, resolved), address) =>
          addresses.get(address) match {
            case Some(pendingAddress) =>
              val x = pendingAddress.withKnownOnNode(txId, restUpdates.getOrElse(address, Map.empty))
              if (x.isResolved) (addresses - address, restUpdates - address, resolved.updated(address, x))
              else (addresses.updated(address, x), restUpdates - address, resolved)
            case None =>
              val x = PendingAddress(
                pendingTxs = Map[ExchangeTransaction.Id, PendingTransactionType](txId -> PendingTransactionType.KnownOnNode),
                stashedBalance = restUpdates.getOrElse(address, Map.empty),
                events = Queue.empty
              )
              (addresses.updated(address, x), restUpdates - address, resolved)
          }
      }

      val (updatedAddresses2, restUpdates2) = restUpdates1.foldLeft((updatedAddresses1, Map.empty: AddressAssets)) {
        case ((addresses, restUpdates), pair @ (address, xs)) =>
          addresses.get(address) match {
            case Some(pendingAddress) => (addresses.updated(address, pendingAddress.withUpdatedBalances(xs)), restUpdates - address)
            case None =>
              if (xs.isEmpty) (addresses, restUpdates)
              else (addresses, restUpdates + pair)
          }
      }

      (
        OrderEventsCoordinatorActorState(updatedAddresses2, knownOnNodeCache.append(txId)._1),
        restUpdates2,
        resolved
      )
    }

}
