package com.wavesplatform.dex.market

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import com.wavesplatform.dex.OrderBookWsState
import com.wavesplatform.dex.api.websockets.WsOrderBook
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized}
import com.wavesplatform.dex.model.{LastTrade, LevelAgg, LevelAmounts, OrderBook, OrderBookAggregatedSnapshot, OrderBookResult, Side}

import scala.collection.immutable.TreeMap
import scala.concurrent.duration.FiniteDuration

object AggregatedOrderBookActor {
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

  case class Settings(wsMessagesInterval: FiniteDuration)

  def apply(settings: Settings, assetPair: AssetPair, amountDecimals: Int, priceDecimals: Int, init: State): Behavior[Message] =
    Behaviors.setup { context =>
      val compile = mkCompile(assetPair, amountDecimals, priceDecimals)(_, _, _)

      def scheduleNextSendWsUpdates(): Cancellable = context.scheduleOnce(settings.wsMessagesInterval, context.self, Command.SendWsUpdates)

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case query: Query =>
              query match {
                case Query.GetMarketStatus(client) =>
                  client ! state.marketStatus
                  Behaviors.same

                case Query.GetAggregatedSnapshot(client) =>
                  client ! state.toOrderBookAggregatedSnapshot
                  Behaviors.same

                case Query.GetHttpView(format, depth, client) =>
                  val key = (format, depth)
                  state.compiledHttpView.get(key) match {
                    case Some(httpResponse) =>
                      client ! httpResponse
                      Behaviors.same

                    case _ =>
                      val httpResponse = compile(state, format, depth)
                      client ! httpResponse
                      default {
                        state.copy(compiledHttpView = state.compiledHttpView.updated(key, httpResponse))
                      }
                  }
              }

            case Command.ApplyChanges(levelChanges, lastTrade, ts) =>
              default {
                state
                  .flushed(levelChanges, lastTrade, ts)
                  .copy(ws = lastTrade.foldLeft(state.ws.withLevelChanges(levelChanges))(_ withLastTrade _))
              }

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

              context.log.trace("[{}] WebSocket connected", client.path.name)
              context.watch(client)
              default(state.copy(ws = state.ws.addSubscription(client)))

            case Command.SendWsUpdates =>
              val updated = state.copy(ws = state.ws.flushed(amountDecimals, priceDecimals, state.asks, state.bids, state.lastUpdateTs))
              if (updated.ws.hasSubscriptions) scheduleNextSendWsUpdates()
              default(updated)
          }
          .receiveSignal { case (_, Terminated(ws)) => default(state.copy(ws = state.ws.withoutSubscription(ws))) }

      default(init)
    }

  def mkCompile(assetPair: AssetPair, amountDecimals: Int, priceDecimals: Int)(state: State, format: DecimalsFormat, depth: Depth): HttpResponse = {
    val assetPairDecimals = format match {
      case Denormalized => Some(amountDecimals -> priceDecimals)
      case _            => None
    }

    val entity =
      OrderBookResult(
        state.lastUpdateTs,
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

  case class State private (
      asks: TreeMap[Price, Amount],
      bids: TreeMap[Price, Amount],
      lastTrade: Option[LastTrade],
      lastUpdateTs: Long,
      compiledHttpView: Map[(DecimalsFormat, Depth), HttpResponse],
      ws: OrderBookWsState
  ) {
    lazy val marketStatus = MarketStatus(
      lastTrade = lastTrade,
      bestBid = bids.headOption.map(toLevelAgg),
      bestAsk = asks.headOption.map(toLevelAgg)
    )

    def flushed(pendingChanges: LevelAmounts, updatedLastTrade: Option[LastTrade], updatedLastUpdateTs: Long): State =
      copy(
        asks = sum(asks, pendingChanges.asks),
        bids = sum(bids, pendingChanges.bids),
        lastTrade = updatedLastTrade.orElse(lastTrade),
        lastUpdateTs = updatedLastUpdateTs,
        compiledHttpView = Map.empty // Could be optimized by depth
      )

    def toOrderBookAggregatedSnapshot = OrderBookAggregatedSnapshot(
      asks = asks.map(toLevelAgg).toSeq,
      bids = bids.map(toLevelAgg).toSeq
    )
  }

  object State {
    val empty = State(
      asks = TreeMap.empty(OrderBook.asksOrdering),
      bids = TreeMap.empty(OrderBook.bidsOrdering),
      lastTrade = None,
      lastUpdateTs = 0,
      compiledHttpView = Map.empty,
      ws = OrderBookWsState(Map.empty, Set.empty, Set.empty, lastTrade = None)
    )

    def fromOrderBook(ob: OrderBook): State = State(
      asks = empty.asks ++ aggregateByPrice(ob.asks), // ++ to preserve an order
      bids = empty.bids ++ aggregateByPrice(ob.bids),
      lastTrade = ob.lastTrade,
      lastUpdateTs = System.currentTimeMillis(), // DEX-642
      compiledHttpView = Map.empty,
      ws = empty.ws
    )
  }

  def toLevelAgg(x: (Price, Amount)): LevelAgg = LevelAgg(x._2, x._1)

  def aggregateByPrice(xs: Side): TreeMap[Price, Amount] = xs.map {
    case (k, v) => k -> v.view.map(_.amount).sum
  }

  def sum(orig: TreeMap[Price, Amount], diff: Map[Price, Amount]): TreeMap[Price, Amount] =
    diff.foldLeft(orig) {
      case (r, (price, amount)) =>
        val updatedAmount = r.getOrElse(price, 0L) + amount
        if (updatedAmount == 0) r - price else r.updated(price, updatedAmount)
    }
}
