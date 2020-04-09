package com.wavesplatform.dex.market

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse}
import cats.kernel.Monoid
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.model.{Amount, Price}
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.market.OrderBookActor.MarketStatus
import com.wavesplatform.dex.model.MatcherModel.{DecimalsFormat, Denormalized}
import com.wavesplatform.dex.model.{LastTrade, LevelAgg, LevelAmounts, OrderBook, OrderBookAggregatedSnapshot, OrderBookResult, Side}

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
  }

  def apply(assetPair: AssetPair, amountDecimals: Int, priceDecimals: Int, init: State): Behavior[Message] =
    Behaviors.setup { _ =>
      val compile = mkCompile(assetPair, amountDecimals, priceDecimals)(_, _, _)
      def default(state: State): Behaviors.Receive[Message] = Behaviors.receiveMessage {
        case Query.GetHttpView(format, depth, client) =>
          val flushed = state.flushed
          val key     = (format, depth)
          // TODO refactor
          val updatedState = flushed.compiledHttpView.get(key) match {
            case Some(r) =>
              client ! r
              flushed

            case _ =>
              val compiledHttpView = compile(flushed, format, depth)
              client ! compiledHttpView
              flushed.copy(compiledHttpView = state.compiledHttpView.updated(key, compiledHttpView))
          }
          log.info(s"[$assetPair] GetHttpView: updatedState.asks:${flushed.asks}, updatedState.bids:${flushed.bids}")
          default(updatedState)

        case Query.GetMarketStatus(client) =>
          val updatedState = state.flushed
          log.info(s"[$assetPair] GetMarketStatus: updatedState.asks:${updatedState.asks}, updatedState.bids:${updatedState.bids}")
          client ! updatedState.marketStatus
          default(updatedState)

        case Query.GetAggregatedSnapshot(client) =>
          val updatedState = state.flushed
          log.info(s"[$assetPair] GetAggregatedSnapshot: updatedState.asks:${updatedState.asks}, updatedState.bids:${updatedState.bids}")
          client ! OrderBookAggregatedSnapshot(
            asks = updatedState.asks.map(State.toLevelAgg).toSeq,
            bids = updatedState.bids.map(State.toLevelAgg).toSeq
          )

          default(updatedState)

        case Command.ApplyChanges(levelChanges, lastTrade, ts) =>
          val updatedPendingChanges = Monoid.combine(state.pendingChanges, levelChanges)
          log.info(s"[$assetPair] ApplyChanges: pendingChanges:${state.pendingChanges} + levelChanges:$levelChanges = $updatedPendingChanges")
          default(
            state.copy(
              lastTrade = lastTrade,
              lastUpdate = ts,
              pendingChanges = updatedPendingChanges
            ))
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
      pendingChanges: LevelAmounts
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
  }

  object State {
    val empty = State(
      asks = TreeMap.empty(OrderBook.asksOrdering),
      bids = TreeMap.empty(OrderBook.bidsOrdering),
      lastTrade = None,
      lastUpdate = 0,
      compiledHttpView = Map.empty,
      pendingChanges = LevelAmounts.empty
    )

    def fromOrderBook(ob: OrderBook): State = State(
      asks = empty.asks ++ sum(ob.asks), // to preserve an order, TODO
      bids = empty.bids ++ sum(ob.bids),
      lastTrade = ob.lastTrade,
      lastUpdate = System.currentTimeMillis(), // TODO
      compiledHttpView = Map.empty,
      pendingChanges = LevelAmounts.empty
    )

    // TODO could it be > Long.Max?
    def sum(xs: Side): TreeMap[Price, Amount] = xs.map {
      case (k, v) => k -> v.view.map(_.amount).sum
    }

    def toLevelAgg(x: (Price, Amount)): LevelAgg = LevelAgg(x._2, x._1)
  }
}
