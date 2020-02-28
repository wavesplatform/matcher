package com.wavesplatform.dex

import akka.actor.ActorRef
import com.wavesplatform.dex.domain.asset.Asset

import scala.collection.immutable.Queue

case class AddressWsMutableState(activeWsConnections: Queue[ActorRef],
                                 pendingWsConnections: Queue[ActorRef],
                                 changedSpendableAssets: Set[Asset],
                                 changedReservableAssets: Set[Asset]) {

  val hasActiveConnections: Boolean = activeWsConnections.nonEmpty
  val hasChangedAssets: Boolean     = getAllAssets.nonEmpty

  def getAllAssets: Set[Asset] = changedSpendableAssets ++ changedReservableAssets

  def addPendingSubscription(subscriber: ActorRef): AddressWsMutableState = copy(pendingWsConnections = pendingWsConnections enqueue subscriber)

  def flushPendingConnections(): AddressWsMutableState =
    copy(activeWsConnections = activeWsConnections ++ pendingWsConnections, pendingWsConnections = Queue.empty)

  def putReservedAssets(diff: Set[Asset]): AddressWsMutableState  = copy(changedReservableAssets = changedReservableAssets ++ diff)
  def putSpendableAssets(diff: Set[Asset]): AddressWsMutableState = copy(changedSpendableAssets = changedSpendableAssets ++ diff)

  def cleanChangedAssets(): AddressWsMutableState = copy(changedSpendableAssets = Set.empty, changedReservableAssets = Set.empty)
}

object AddressWsMutableState {
  val empty: AddressWsMutableState = AddressWsMutableState(Queue.empty, Queue.empty, Set.empty, Set.empty)
}
