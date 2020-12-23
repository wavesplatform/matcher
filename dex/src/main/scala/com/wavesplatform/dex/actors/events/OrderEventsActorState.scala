package com.wavesplatform.dex.actors.events

import cats.syntax.option._
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.model.Events

import scala.collection.immutable.Queue

// TODO DEX-1041
case class OrderEventsActorState(addresses: Map[Address, PendingAddress], knownOnNodeCache: FifoSet[ExchangeTransaction.Id]) {

  /**
   * @return (updated, passUpdates), passUpdates can be sent to recipients
   */
  def withBalanceUpdates(updates: AddressAssets): (OrderEventsActorState, AddressAssets) = {
    // TODO DEX-1041 probably, we need fold on addresses, because updates.size >> addresses.size
    val (updatedAddresses, passUpdates) = updates.foldLeft((addresses, Map.empty: AddressAssets)) {
      case ((addresses, passUpdates), item @ (address, updates)) =>
        addresses.get(address) match {
          case Some(x) => (addresses.updated(address, x.withUpdatedBalances(updates)), passUpdates)
          case None => (addresses, passUpdates + item)
        }
    }

    (OrderEventsActorState(updatedAddresses, knownOnNodeCache), passUpdates)
  }

  /**
   * @return (updated, passEvent), passEvent can be sent to a recipient
   */
  def withPendingCancel(event: Events.OrderCanceled): (OrderEventsActorState, Option[Events.OrderCanceled]) = {
    val address = event.acceptedOrder.order.senderPublicKey.toAddress
    addresses.get(address) match {
      case Some(x) => (OrderEventsActorState(addresses.updated(address, x.withEvent(event)), knownOnNodeCache), none)
      case _ => (this, event.some)
    }
  }

  /**
   * @return (updated, resolved)
   */
  def withExecuted(txId: ExchangeTransaction.Id, event: Events.OrderExecuted): (OrderEventsActorState, Map[Address, PendingAddress]) = {
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

    (OrderEventsActorState(updatedAddresses, knownOnNodeCache), resolved)
  }

  /**
   * @return (updated, passUpdates, resolved)
   */
  def withKnownOnNodeTx(
    traders: Set[Address],
    txId: ExchangeTransaction.Id,
    balanceUpdates: AddressAssets
  ): (OrderEventsActorState, AddressAssets, Map[Address, PendingAddress]) =
    if (knownOnNodeCache.contains(txId)) (this, balanceUpdates, Map.empty)
    else {
      val (updatedAddresses, restUpdates, resolved) = traders.foldLeft((addresses, balanceUpdates, Map.empty[Address, PendingAddress])) {
        case (r @ (addresses, restUpdates, resolved), address) =>
          addresses.get(address) match {
            case None => r
            case Some(pendingAddress) =>
              val x = pendingAddress.withKnownOnNode(txId, restUpdates.getOrElse(address, Map.empty))
              if (x.isResolved) (addresses - address, restUpdates - address, resolved.updated(address, x))
              else (addresses.updated(address, x), restUpdates - address, resolved)
          }
      }

      (
        OrderEventsActorState(updatedAddresses, knownOnNodeCache.append(txId)),
        restUpdates,
        resolved
      )
    }

}
