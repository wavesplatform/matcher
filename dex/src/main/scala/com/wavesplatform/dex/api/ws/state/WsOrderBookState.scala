package com.wavesplatform.dex.api.ws.state

import akka.actor.typed.ActorRef
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.entities.{WsLastTrade, WsOrderBookSettings}
import com.wavesplatform.dex.api.ws.protocol
import com.wavesplatform.dex.api.ws.protocol.WsOrderBookChanges
import com.wavesplatform.dex.api.ws.state.WsAddressState.getNextUpdateId
import com.wavesplatform.dex.api.ws.state.WsOrderBookState._
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.Denormalization.{denormalizeAmountAndFee, denormalizePrice}
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts, OrderBook}
import monocle.macros.GenLens

import scala.collection.immutable.TreeMap

case class WsOrderBookState(
  wsConnections: Map[ActorRef[WsOrderBookChanges], Long],
  changedAsks: Set[Price],
  changedBids: Set[Price],
  lastTrade: Option[LastTrade],
  changedTickSize: Option[Double],
  prevTickState: WsState
) {

  def addSubscription(x: ActorRef[WsOrderBookChanges]): WsOrderBookState = copy(wsConnections = wsConnections.updated(x, 0L))

  def withoutSubscription(x: ActorRef[WsOrderBookChanges]): WsOrderBookState = {
    val updatedConnections = wsConnections - x
    if (updatedConnections.isEmpty) WsOrderBookState.empty.copy(prevTickState = prevTickState)
    else copy(wsConnections = updatedConnections)
  }

  def hasSubscriptions: Boolean = wsConnections.nonEmpty

  def denormalized(amountDecimals: Int, priceDecimals: Int, xs: TreeMap[Price, Amount], ordering: Ordering[Double]): TreeMap[Double, Double] =
    xs.map {
      case (price, amount) =>
        denormalizePrice(price, amountDecimals, priceDecimals).toDouble -> denormalizeAmountAndFee(amount, amountDecimals).toDouble
    }(ordering)

  def lastTrade(amountDecimals: Int, priceDecimals: Int, x: LastTrade): WsLastTrade = WsLastTrade(
    price = denormalizePrice(x.price, amountDecimals, priceDecimals).toDouble,
    amount = denormalizeAmountAndFee(x.amount, amountDecimals).toDouble,
    side = x.side
  )

  def flushed(
    assetPair: AssetPair,
    amountDecimals: Int,
    priceDecimals: Int,
    asks: TreeMap[Price, Amount],
    bids: TreeMap[Price, Amount],
    timestamp: Long
  ): WsOrderBookState = copy(
    wsConnections = {
      val preparedAsks = take(asks, changedAsks, prevTickState.asks)
      val preparedBids = take(bids, changedBids, prevTickState.bids)

      if (lastTrade.nonEmpty || changedTickSize.nonEmpty || preparedBids.nonEmpty || preparedAsks.nonEmpty) {
        val changes =
          protocol.WsOrderBookChanges(
            assetPair = assetPair,
            asks = denormalized(amountDecimals, priceDecimals, preparedAsks, OrderBook.asksDenormalizedOrdering),
            bids = denormalized(amountDecimals, priceDecimals, preparedBids, OrderBook.bidsDenormalizedOrdering),
            lastTrade = lastTrade.map(lastTrade(amountDecimals, priceDecimals, _)),
            updateId = 0L, // Will be changed below
            timestamp = timestamp,
            settings = if (changedTickSize.isDefined) WsOrderBookSettings(None, changedTickSize).some else None
          )
        wsConnections.map {
          case (conn, updateId) =>
            val newUpdateId = getNextUpdateId(updateId)
            conn ! changes.copy(updateId = newUpdateId)
            conn -> newUpdateId
        }
      } else wsConnections
    },
    changedAsks = Set.empty,
    changedBids = Set.empty,
    lastTrade = None,
    changedTickSize = None,
    prevTickState = updatePrevTickState(asks, bids, lastTrade, changedTickSize)
  )

  def updatePrevTickState(asks: Map[Price, Amount], bids: Map[Price, Amount], lt: Option[LastTrade], lc: Option[Double]): WsState =
    prevTickState.copy(
      asks = applyLevelChanges(prevTickState.asks, asks),
      bids = applyLevelChanges(prevTickState.bids, bids),
      lastTrade = if (lt.isEmpty) prevTickState.lastTrade else lt,
      tickSize = if (lc.isEmpty) prevTickState.tickSize else lc
    )

  def applyLevelChanges(m1: TreeMap[Price, Amount], m2: Map[Price, Amount]): TreeMap[Price, Amount] = (m1 ++ m2.map {
    case (pr, am) =>
      (pr, m1.get(pr).fold(am)(_ + am))
  }).filter(_._2 != 0)

  def take(xs: TreeMap[Price, Amount], levels: Set[Price], prevChanges: TreeMap[Price, Amount]): TreeMap[Price, Amount] = {
    // 1. Levels will be always smaller, than xs
    // 2. A level could gone from xs
    val r = TreeMap.newBuilder[Price, Amount](xs.ordering)
    levels.foreach { level =>
      xs.get(level).fold {
        prevChanges.get(level).foreach(_ => r += level -> 0L) // means that level was removed
      } { v =>
        if (!prevChanges.get(level).contains(v)) // if there was no changes to this level, then don't send it
          r += level -> v
      }

    }
    r.result()
  }

  def accumulateChanges(lc: LevelAmounts, lt: Option[LastTrade], ts: Option[Double]): WsOrderBookState =
    if (hasSubscriptions)
      (
        changedAsksLens.modify(_ ++ lc.asks.keySet) andThen
        changedBidsLens.modify(_ ++ lc.bids.keySet) andThen
        lastTradeLens.modify(if (lt.isEmpty) _ else lt) andThen
        changedTickSizeLens.modify(if (ts.isEmpty) _ else ts)
      )(this)
    else
      copy(
        prevTickState = updatePrevTickState(lc.asks, lc.bids, lt, ts)
      )

}

object WsOrderBookState {
  val empty = WsOrderBookState(Map.empty, Set.empty, Set.empty, None, None, WsState.empty)

  def withPrevTickState(
    asks: TreeMap[Price, Amount],
    bids: TreeMap[Price, Amount],
    lastTrade: Option[LastTrade],
    tickSize: Option[Double]
  ): WsOrderBookState = empty.copy(prevTickState = WsState(asks, bids, lastTrade, tickSize))

  val genLens = GenLens[WsOrderBookState]
  val changedAsksLens = genLens(_.changedAsks)
  val changedBidsLens = genLens(_.changedBids)
  val lastTradeLens = genLens(_.lastTrade)
  val changedTickSizeLens = genLens(_.changedTickSize)

  final private[WsOrderBookState] case class WsState(
    asks: TreeMap[Price, Amount],
    bids: TreeMap[Price, Amount],
    lastTrade: Option[LastTrade],
    tickSize: Option[Double]
  )

  object WsState {
    val empty = WsState(TreeMap.empty, TreeMap.empty, None, None)
  }

}
