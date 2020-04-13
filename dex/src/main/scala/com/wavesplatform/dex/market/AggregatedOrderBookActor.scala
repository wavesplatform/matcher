package com.wavesplatform.dex.market

import java.util.UUID

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import cats.kernel.Monoid
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
    case class AddWsSubscription(client: ActorRef[WsOrderBook], id: UUID)                       extends Command
    private[AggregatedOrderBookActor] case object SendWsUpdates                                 extends Command
  }

  def apply(settings: OrderBookActor.Settings, assetPair: AssetPair, amountDecimals: Int, priceDecimals: Int, init: State): Behavior[Message] =
    Behaviors.setup { context =>
      val compile   = mkCompile(assetPair, amountDecimals, priceDecimals)(_, _, _)
      val wsUpdates = new WsOrderBook.Update(amountDecimals, priceDecimals: Int)

      def scheduleNextSendWsUpdates(): Cancellable = context.scheduleOnce(settings.wsMessagesInterval, context.self, Command.SendWsUpdates)

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case query: Query =>
              val flushed = state.flushed
              val updatedState = query match {
                case Query.GetMarketStatus(client)       => flushed.unsafeTap(client ! _.marketStatus)
                case Query.GetAggregatedSnapshot(client) => flushed.unsafeTap(client ! _.toOrderBookAggregatedSnapshot)
                case Query.GetHttpView(format, depth, client) =>
                  val key = (format, depth)
                  val (updatedState, httpResponse) = flushed.compiledHttpView.get(key) match {
                    case Some(r) => (flushed, r)
                    case _ =>
                      val r = compile(flushed, format, depth)
                      (flushed.copy(compiledHttpView = flushed.compiledHttpView.updated(key, r)), r)
                  }
                  client ! httpResponse
                  updatedState
              }
              log.info(s"[$assetPair] $query: updatedState.asks:${updatedState.asks}, updatedState.bids:${updatedState.bids}")
              default(updatedState)

            case Command.ApplyChanges(levelChanges, lastTrade, ts) =>
              val updatedPendingChanges = Monoid.combine(state.pendingChanges, levelChanges)
              log.info(s"[$assetPair] ApplyChanges: pendingChanges:${state.pendingChanges} + levelChanges:$levelChanges = $updatedPendingChanges")
              default(
                state.copy(
                  lastTrade = lastTrade,
                  lastUpdate = ts,
                  pendingChanges = updatedPendingChanges,
                  ws = lastTrade.foldLeft(state.ws.withLevelChanges(wsUpdates, levelChanges))(_.withLastTrade(wsUpdates, _))
                ))

            case Command.AddWsSubscription(client, id) =>
              val flushed = state.flushed

              if (!flushed.ws.hasSubscriptions) scheduleNextSendWsUpdates()
              val ob = flushed.toOrderBookAggregatedSnapshot
              client ! wsUpdates.from(
                asks = ob.asks,
                bids = ob.bids,
                lt = flushed.lastTrade,
                updateId = 0L
              )

              log.trace(s"[$id, name=${client.path.name}] WebSocket connected")
              context.watch(client)
              default(flushed.copy(ws = flushed.ws.addSubscription(client, id)))

            case Command.SendWsUpdates =>
              val flushed = state.copy(ws = state.ws.flushed())
              if (flushed.ws.hasSubscriptions) scheduleNextSendWsUpdates()
              default(flushed)
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
      pendingChanges: LevelAmounts,
      ws: OrderBookWsState
  ) {
    lazy val marketStatus = MarketStatus(
      lastTrade = lastTrade,
      bestBid = bids.headOption.map(State.toLevelAgg),
      bestAsk = asks.headOption.map(State.toLevelAgg)
    )

    def flushed: State =
      if (this.pendingChanges.isEmpty) this
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
          pendingChanges = LevelAmounts.empty,
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
      pendingChanges = LevelAmounts.empty,
      ws = OrderBookWsState(Map.empty, WsOrderBook.empty)
    )

    def fromOrderBook(ob: OrderBook): State = State(
      asks = empty.asks ++ sum(ob.asks), // to preserve an order, TODO
      bids = empty.bids ++ sum(ob.bids),
      lastTrade = ob.lastTrade,
      lastUpdate = System.currentTimeMillis(), // TODO
      compiledHttpView = Map.empty,
      pendingChanges = LevelAmounts.empty,
      ws = empty.ws
    )

    // TODO could it be > Long.Max?
    def sum(xs: Side): TreeMap[Price, Amount] = xs.map {
      case (k, v) => k -> v.view.map(_.amount).sum
    }

    def toLevelAgg(x: (Price, Amount)): LevelAgg = LevelAgg(x._2, x._1)
  }
}
