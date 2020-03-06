package com.wavesplatform.dex

import akka.actor.ActorRef
import com.wavesplatform.dex.api.websockets.WsOrderBook
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts}

import scala.collection.immutable.Queue

case class OrderBookWsState(update: WsOrderBook.Update, wsConnections: Queue[ActorRef], changes: WsOrderBook) {
  def addSubscription(x: ActorRef): OrderBookWsState = copy(wsConnections = wsConnections.enqueue(x))
  def hasSubscriptions: Boolean                      = wsConnections.nonEmpty

  def withLastTrade(x: LastTrade): OrderBookWsState = if (wsConnections.isEmpty) this else copy(changes = update.withLastTrade(changes, x))
  def withLevelChanges(xs: LevelAmounts): OrderBookWsState =
    if (wsConnections.isEmpty) this else copy(changes = update.withLevelChanges(changes, xs))

  def flushed(): OrderBookWsState = {
    wsConnections.foreach(_ ! changes)
    copy(changes = WsOrderBook.empty)
  }

  def hasChanges: Boolean = changes.nonEmpty
}
