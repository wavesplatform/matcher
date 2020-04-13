package com.wavesplatform.dex.market

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import com.wavesplatform.dex.OrderBookWsState
import com.wavesplatform.dex.api.websockets.WsOrderBook
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized}
import com.wavesplatform.dex.model.{LastTrade, LevelAgg, LevelAmounts, OrderBook, OrderBookAggregatedSnapshot, OrderBookResult, Side}
import mouse.any.anySyntaxMouse

import scala.collection.immutable.TreeMap

object AggregatedOrderBookActor extends ScorexLogging {
  type Depth = Int

  sealed trait Message extends Product with Serializable

  sealed trait Query extends Message
  object Query {
    case class GetHttpView(format: DecimalsFormat, depth: Depth, client: ActorRef[HttpResponse]) extends Query
    case class GetMarketStatus(client: ActorRef[MarketStatus])                                   extends Query
    case class GetAggregatedSnapshot(client: ActorRef[OrderBookAggregatedSnapshot])              extends Query
  }

  sealed trait Command extends Message
  object Command {
    case class ApplyChanges(levelChanges: LevelAmounts, lastTrade: Option[LastTrade], ts: Long) extends Command
    case class AddWsSubscription(client: ActorRef[WsOrderBook])                                 extends Command
    private[AggregatedOrderBookActor] case object SendWsUpdates                                 extends Command
  }

  def apply(settings: OrderBookActor.Settings, assetPair: AssetPair, amountDecimals: Int, priceDecimals: Int, init: State): Behavior[Message] =
    Behaviors.setup { context =>
      val compile = mkCompile(assetPair, amountDecimals, priceDecimals)(_, _, _)

      def scheduleNextSendWsUpdates(): Cancellable = context.scheduleOnce(settings.wsMessagesInterval, context.self, Command.SendWsUpdates)

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case query: Query =>
              val updatedState = query match {
                case Query.GetMarketStatus(client)       => state.unsafeTap(client ! _.marketStatus)
                case Query.GetAggregatedSnapshot(client) => state.unsafeTap(client ! _.toOrderBookAggregatedSnapshot)
                case Query.GetHttpView(format, depth, client) =>
                  val key = (format, depth)
                  val (updatedState, httpResponse) = state.compiledHttpView.get(key) match {
                    case Some(r) => (state, r)
                    case _ =>
                      val r = compile(state, format, depth)
                      (state.copy(compiledHttpView = state.compiledHttpView.updated(key, r)), r)
                  }
                  client ! httpResponse
                  updatedState
              }
              log.info(s"[$assetPair] $query: updatedState.asks:${updatedState.asks}, updatedState.bids:${updatedState.bids}")
              default(updatedState)

            case Command.ApplyChanges(levelChanges, lastTrade, ts) =>
              val flushed = state
                .copy(
                  lastTrade = lastTrade.orElse(state.lastTrade),
                  lastUpdate = ts,
                  ws = lastTrade.foldLeft(state.ws.withLevelChanges(levelChanges))(_ withLastTrade _)
                )
                .flushed(levelChanges)

              log.info(
                s"[$assetPair] ApplyChanges: pendingChanges.asks:${state.asks}, bids:${state.bids} + levelChanges:$levelChanges = flushed.asks:${flushed.asks}, bids:${flushed.bids}, lastTrade:$lastTrade")
              log.info(
                s"[$assetPair] updated .ws.changedAsks:${flushed.ws.changedAsks}, .ws.changedBids:${flushed.ws.changedBids}, .ws.lastTrade:${flushed.ws.lastTrade}")
              default(flushed)

            case Command.AddWsSubscription(client) =>
              if (!state.ws.hasSubscriptions) scheduleNextSendWsUpdates()
              val ob = state.toOrderBookAggregatedSnapshot
              client ! WsOrderBook.from(
                amountDecimals = amountDecimals,
                priceDecimals = priceDecimals,
                asks = ob.asks,
                bids = ob.bids,
                lt = state.lastTrade,
                updateId = 0L
              )

              log.trace(s"[${client.path.name}] WebSocket connected")
              context.watch(client)
              default(state.copy(ws = state.ws.addSubscription(client)))

            case Command.SendWsUpdates =>
              val updated = state.copy(ws = state.ws.flushed(amountDecimals, priceDecimals, state.asks, state.bids))
              if (updated.ws.hasSubscriptions) scheduleNextSendWsUpdates()
              default(updated)
          }
          .receiveSignal {
            case (context, Terminated(ws)) =>
              // TODO move to context.log
              log.trace(s"[${ws.path.name}] WebSocket terminated")
              default(state.copy(ws = state.ws.withoutSubscription(ws)))
          }

      default(init)
    }

  def mkCompile(assetPair: AssetPair, amountDecimals: Int, priceDecimals: Int)(state: State, format: DecimalsFormat, depth: Depth): HttpResponse = {
    val assetPairDecimals = format match {
      case Denormalized => Some(amountDecimals -> priceDecimals)
      case _            => None
    }

    val entity =
      OrderBookResult(
        state.lastUpdate,
        assetPair,
        state.bids.take(depth).map { case (price, amount) => LevelAgg(amount, price) }.toList,
        state.asks.take(depth).map { case (price, amount) => LevelAgg(amount, price) }.toList,
        assetPairDecimals
      )

    HttpResponse(
      entity = HttpEntity(
        ContentTypes.`application/json`,
        OrderBookResult.toJson(entity)
      )
    )
  }

  case class State(
      asks: TreeMap[Price, Amount],
      bids: TreeMap[Price, Amount],
      lastTrade: Option[LastTrade],
      lastUpdate: Long,
      compiledHttpView: Map[(DecimalsFormat, Depth), HttpResponse],
      ws: OrderBookWsState
  ) {
    lazy val marketStatus = MarketStatus(
      lastTrade = lastTrade,
      bestBid = bids.headOption.map(State.toLevelAgg),
      bestAsk = asks.headOption.map(State.toLevelAgg)
    )

    def flushed(pendingChanges: LevelAmounts): State =
      if (pendingChanges.isEmpty) this
      else
        copy(
          // TODO optimize (save order)
          asks = pendingChanges.asks.foldLeft(asks) {
            case (r, (price, amount)) =>
              val updatedAmount = r.getOrElse(price, 0L) + amount
              if (updatedAmount == 0) r - price else r.updated(price, updatedAmount)
          },
          bids = pendingChanges.bids.foldLeft(bids) {
            case (r, (price, amount)) =>
              val updatedAmount = r.getOrElse(price, 0L) + amount
              if (updatedAmount == 0) r - price else r.updated(price, updatedAmount)
          },
          compiledHttpView = Map.empty // Could be optimized by depth
        )

    def toOrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot(
      asks = asks.map(State.toLevelAgg).toSeq,
      bids = bids.map(State.toLevelAgg).toSeq
    )
  }

  object State {
    val empty = State(
      asks = TreeMap.empty(OrderBook.asksOrdering),
      bids = TreeMap.empty(OrderBook.bidsOrdering),
      lastTrade = None,
      lastUpdate = 0,
      compiledHttpView = Map.empty,
      ws = OrderBookWsState(
        Map.empty,
        Set.empty,
        Set.empty,
        lastTrade = None,
        timestamp = System.currentTimeMillis()
      )
    )

    def fromOrderBook(ob: OrderBook): State = State(
      asks = empty.asks ++ sum(ob.asks), // to preserve an order, TODO
      bids = empty.bids ++ sum(ob.bids),
      lastTrade = ob.lastTrade,
      lastUpdate = System.currentTimeMillis(), // TODO
      compiledHttpView = Map.empty,
      ws = empty.ws
    )

    // TODO could it be > Long.Max?
    def sum(xs: Side): TreeMap[Price, Amount] = xs.map {
      case (k, v) => k -> v.view.map(_.amount).sum
    }

    def toLevelAgg(x: (Price, Amount)): LevelAgg = LevelAgg(x._2, x._1)
  }
}
