package com.wavesplatform.dex

import akka.actor.typed.ActorRef
import cats.syntax.option._
import com.wavesplatform.dex.AddressWsMutableState.getNextUpdateId
import com.wavesplatform.dex.api.websockets.{WsLastTrade, WsOrderBook, WsOrderBookSettings}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.Denormalization.{denormalizeAmountAndFee, denormalizePrice}
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.model.{LastTrade, LevelAmounts}
import monocle.macros.GenLens

import scala.collection.immutable.TreeMap

case class OrderBookWsState(wsConnections: Map[ActorRef[WsOrderBook], Long],
                            changedAsks: Set[Price],
                            changedBids: Set[Price],
                            lastTrade: Option[LastTrade],
                            changedTickSize: Option[Double]) {

  private val genLens: GenLens[OrderBookWsState] = GenLens[OrderBookWsState]

  def addSubscription(x: ActorRef[WsOrderBook]): OrderBookWsState = copy(wsConnections = wsConnections.updated(x, 0L))

  def withoutSubscription(x: ActorRef[Nothing]): OrderBookWsState =
    if (wsConnections.size == 1) OrderBookWsState(Map.empty, Set.empty, Set.empty, None, None)
    else copy(wsConnections = wsConnections.filterKeys(_ != x))

  def hasSubscriptions: Boolean = wsConnections.nonEmpty

  def hasChanges: Boolean = changedAsks.nonEmpty || changedBids.nonEmpty || lastTrade.nonEmpty || changedTickSize.nonEmpty

  def denormalized(amountDecimals: Int, priceDecimals: Int, xs: TreeMap[Price, Amount]): TreeMap[Double, Double] = xs.map {
    case (price, amount) =>
      denormalizePrice(price, amountDecimals, priceDecimals).toDouble -> denormalizeAmountAndFee(amount, amountDecimals).toDouble
  }

  def lastTrade(amountDecimals: Int, priceDecimals: Int, x: LastTrade): WsLastTrade = WsLastTrade(
    price = denormalizePrice(x.price, amountDecimals, priceDecimals).toDouble,
    amount = denormalizeAmountAndFee(x.amount, amountDecimals).toDouble,
    side = x.side
  )

  def flushed(assetPair: AssetPair,
              amountDecimals: Int,
              priceDecimals: Int,
              asks: TreeMap[Price, Amount],
              bids: TreeMap[Price, Amount],
              timestamp: Long): OrderBookWsState = copy(
    wsConnections = if (hasChanges) {
      val changes =
        WsOrderBook(
          assetPair = assetPair,
          asks = denormalized(amountDecimals, priceDecimals, take(asks, changedAsks)),
          bids = denormalized(amountDecimals, priceDecimals, take(bids, changedBids)),
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
    } else wsConnections,
    changedAsks = Set.empty,
    changedBids = Set.empty,
    lastTrade = None,
    changedTickSize = None
  )

  def take(xs: TreeMap[Price, Amount], levels: Set[Price]): TreeMap[Price, Amount] = {
    // 1. Levels will be always smaller, than xs
    // 2. A level could gone from xs
    val r = TreeMap.newBuilder[Price, Amount](xs.ordering)
    levels.foreach { level =>
      r += level -> xs.getOrElse(level, 0L)
    }
    r.result()
  }

  def accumulateChanges(lc: LevelAmounts, lt: Option[LastTrade], ts: Option[Double]): OrderBookWsState =
    if (hasSubscriptions) {
      (
        genLens(_.changedAsks).modify(_ ++ lc.asks.keySet) andThen
          genLens(_.changedBids).modify(_ ++ lc.bids.keySet) andThen
          genLens(_.lastTrade).modify { if (lt.isEmpty) _ else lt } andThen
          genLens(_.changedTickSize).modify { if (ts.isEmpty) _ else ts }
      )(this)
    } else this
}
