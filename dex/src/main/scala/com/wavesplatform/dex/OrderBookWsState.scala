package com.wavesplatform.dex

import java.util.UUID

import akka.actor.ActorRef
import com.wavesplatform.dex.AddressWsMutableState.getNextUpdateId
import com.wavesplatform.dex.api.websockets.WsOrderBook
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts}

case class OrderBookWsState(update: WsOrderBook.Update, wsConnections: Map[ActorRef, (UUID, Long)], changes: WsOrderBook) {

  def addSubscription(x: ActorRef, id: UUID): OrderBookWsState = copy(wsConnections = wsConnections + (x -> (id -> 0L)))

  def withoutSubscription(x: ActorRef): OrderBookWsState =
    if (wsConnections.size == 1) copy(wsConnections = Map.empty, changes = WsOrderBook.empty)
    else copy(wsConnections = wsConnections.filterKeys(_ != x))

  def withoutSubscriptions: OrderBookWsState = copy(wsConnections = Map.empty)

  def hasSubscriptions: Boolean = wsConnections.nonEmpty

  def withLastTrade(x: LastTrade): OrderBookWsState        = if (hasSubscriptions) copy(changes = update.withLastTrade(changes, x)) else this
  def withLevelChanges(xs: LevelAmounts): OrderBookWsState = if (hasSubscriptions) copy(changes = update.withLevelChanges(changes, xs)) else this

  def flushed(): OrderBookWsState = copy(
    wsConnections = if (changes.nonEmpty) {
      wsConnections.map {
        case (conn, (connId, updateId)) =>
          val newUpdateId = getNextUpdateId(updateId)
          conn ! changes.copy(updateId = newUpdateId)
          conn -> (connId -> newUpdateId)
      }
    } else wsConnections,
    changes = WsOrderBook.empty
  )

  def hasChanges: Boolean = changes.nonEmpty
}
