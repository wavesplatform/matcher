package com.wavesplatform.dex

import akka.actor.typed.ActorRef
import com.wavesplatform.dex.AddressWsMutableState.getNextUpdateId
import com.wavesplatform.dex.api.websockets.{WsLastTrade, WsOrderBook}
import com.wavesplatform.dex.domain.model.Denormalization.{denormalizeAmountAndFee, denormalizePrice}
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts, OrderBook}

import scala.collection.immutable.TreeMap

case class OrderBookWsState(wsConnections: Map[ActorRef[WsOrderBook], Long],
                            changedAsks: Set[Price],
                            changedBids: Set[Price],
                            lastTrade: Option[LastTrade],
                            timestamp: Long = System.currentTimeMillis) {

  def addSubscription(x: ActorRef[WsOrderBook]): OrderBookWsState = copy(wsConnections = wsConnections.updated(x, 0L))

  def withoutSubscription(x: ActorRef[Nothing]): OrderBookWsState =
    if (wsConnections.size == 1) OrderBookWsState(Map.empty, Set.empty, Set.empty, None)
    else copy(wsConnections = wsConnections.filterKeys(_ != x))

  def withoutSubscriptions: OrderBookWsState = copy(wsConnections = Map.empty)

  def hasSubscriptions: Boolean = wsConnections.nonEmpty

  def hasChanges: Boolean = changedAsks.nonEmpty || changedBids.nonEmpty || lastTrade.nonEmpty

  def withLastTrade(x: LastTrade): OrderBookWsState =
    if (hasSubscriptions) copy(lastTrade = Some(x)) else this

  def withLevelChanges(xs: LevelAmounts): OrderBookWsState =
    if (hasSubscriptions)
      copy(
        changedAsks = changedAsks ++ xs.asks.keySet,
        changedBids = changedBids ++ xs.bids.keySet,
        timestamp = System.currentTimeMillis // TODO
      )
    else this

  def denormalized(amountDecimals: Int, priceDecimals: Int, xs: TreeMap[Price, Amount]): TreeMap[Double, Double] = xs.map {
    case (price, amount) =>
      denormalizePrice(price, amountDecimals, priceDecimals).toDouble -> denormalizeAmountAndFee(amount, amountDecimals).toDouble
  }

  def lastTrade(amountDecimals: Int, priceDecimals: Int, x: LastTrade): WsLastTrade = WsLastTrade(
    price = denormalizePrice(x.price, amountDecimals, priceDecimals).toDouble,
    amount = denormalizeAmountAndFee(x.amount, amountDecimals).toDouble,
    side = x.side
  )

  def flushed(amountDecimals: Int, priceDecimals: Int, asks: TreeMap[Price, Amount], bids: TreeMap[Price, Amount]): OrderBookWsState = copy(
    wsConnections = if (hasChanges) {
      val changes = WsOrderBook(
        asks = denormalized(amountDecimals, priceDecimals, changedAsks.foldLeft(TreeMap.empty[Price, Amount](OrderBook.asksOrdering))((r, x) => r.updated(x, asks.getOrElse(x, 0L)))),
        bids = denormalized(amountDecimals, priceDecimals, changedBids.foldLeft(TreeMap.empty[Price, Amount](OrderBook.bidsOrdering))((r, x) => r.updated(x, bids.getOrElse(x, 0L)))),
        lastTrade = lastTrade.map(lastTrade(amountDecimals, priceDecimals, _)),
        updateId = 0L,
        timestamp = System.currentTimeMillis() // TODO
      )
      wsConnections.map {
        case (conn, updateId) =>
          val newUpdateId = getNextUpdateId(updateId)
          conn ! changes.copy(updateId = newUpdateId)
          conn -> newUpdateId
      }
    } else wsConnections,
    changedAsks = Set.empty,
    changedBids = Set.empty,
    lastTrade = None,
    timestamp = System.currentTimeMillis() // TODO
  )
}
