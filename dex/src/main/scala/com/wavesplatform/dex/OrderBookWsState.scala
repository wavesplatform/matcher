package com.wavesplatform.dex

import akka.actor.ActorRef
import com.wavesplatform.dex.api.websockets.WsOrderBookState

import scala.collection.immutable.Queue

case class OrderBookWsState(activeWsConnections: Queue[ActorRef], changes: WsOrderBookState) {
  def addSubscription(x: ActorRef): OrderBookWsState = copy(activeWsConnections = activeWsConnections.enqueue(x))

  def withLevelChanges(): OrderBookWsState = ???
  def cleanedChanges = copy(changes = WsOrderBookState.empty)
}
