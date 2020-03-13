package com.wavesplatform.dex

import akka.actor.ActorRef
import com.wavesplatform.dex.api.websockets.WsOrderBook
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts}

import scala.collection.immutable.Queue

case class OrderBookWsState(update: WsOrderBook.Update, wsConnections: Queue[ActorRef], changes: WsOrderBook) {
  def addSubscription(x: ActorRef): OrderBookWsState = copy(wsConnections = wsConnections.enqueue(x))

  def withoutSubscription(x: ActorRef): OrderBookWsState =
    if (wsConnections.lengthCompare(1) == 0) copy(wsConnections = Queue.empty, changes = WsOrderBook.empty)
    else copy(wsConnections = wsConnections.filterNot(_ == x))

  def withoutSubscriptions: OrderBookWsState = copy(wsConnections = Queue.empty)

  def hasSubscriptions: Boolean = wsConnections.nonEmpty

  def withLastTrade(x: LastTrade): OrderBookWsState = if (hasSubscriptions) copy(changes = update.withLastTrade(changes, x)) else this
  def withLevelChanges(xs: LevelAmounts): OrderBookWsState =
    if (hasSubscriptions) copy(changes = update.withLevelChanges(changes, xs)) else this

  def flushed(): OrderBookWsState = {
    if (changes.nonEmpty) wsConnections.foreach(_ ! changes)
    copy(changes = WsOrderBook.empty)
  }

  def hasChanges: Boolean = changes.nonEmpty
}
