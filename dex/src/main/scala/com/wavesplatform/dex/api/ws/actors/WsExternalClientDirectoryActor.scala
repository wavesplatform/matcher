package com.wavesplatform.dex.api.ws.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import com.wavesplatform.dex.api.ws.protocol.WsRatesUpdates
import com.wavesplatform.dex.api.ws.state.WsAddressState
import com.wavesplatform.dex.caches.RateCache
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.error
import com.wavesplatform.dex.time.Time

import scala.collection.immutable.HashMap

/**
  * Contains subscriptions for internal stream and broadcast messages.
  */
object WsExternalClientDirectoryActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class Subscribe(clientRef: ActorRef[WsExternalClientHandlerActor.Message]) extends Command
    case class CloseOldest(number: Int)                                             extends Command
    case class BroadcastRatesUpdates(newRates: Map[Asset, Double])                  extends Command
  }

  sealed trait Query extends Message
  object Query {
    case class GetActiveNumber(client: ActorRef[Int]) extends Query
  }

  def apply(rateCache: RateCache, time: Time): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.system.eventStream

      def matcherTime: Long = time.getTimestamp()

      def mkRatesSnapshot = WsRatesUpdates(rateCache.getAllRates, 0, matcherTime)

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case Command.Subscribe(clientRef) =>
              context.watch(clientRef)
              clientRef ! WsExternalClientHandlerActor.Command.ForwardToClient(mkRatesSnapshot)
              default(state withActor clientRef)

            case Command.CloseOldest(n) =>
              val (updatedState, oldest) = state.withoutOldest(n)
              oldest.iterator.foreach(_ ! WsExternalClientHandlerActor.Command.CloseConnection(error.Balancing))
              default(updatedState)

            case Command.BroadcastRatesUpdates(newRates) =>
              default {
                state.copy(
                  all = state.all.map {
                    case (target, meta) =>
                      val newUpdateId = WsAddressState.getNextUpdateId(meta.ratesUpdateId)
                      target ! WsExternalClientHandlerActor.Command.ForwardToClient(WsRatesUpdates(newRates, newUpdateId, matcherTime))
                      target -> meta.copy(ratesUpdateId = newUpdateId)
                  }
                )
              }

            case Query.GetActiveNumber(client) =>
              client ! state.all.size
              default(state)
          }
          .receiveSignal {
            case (_, Terminated(clientRef)) =>
              default {
                state.withoutActor(clientRef.unsafeUpcast: TargetActor)
              }
          }

      default(State(0, HashMap.empty))
    }

  private type TargetActor = ActorRef[WsExternalClientHandlerActor.Message]
  private type Index       = Int

  private case class TargetActorMeta(index: Index, ratesUpdateId: Long)

  private case class State(currentIndex: Index, all: HashMap[TargetActor, TargetActorMeta]) {

    def withActor(x: TargetActor): State = copy(
      currentIndex = currentIndex + 1,
      all = all.updated(x, TargetActorMeta(currentIndex, 0L))
    )

    def withoutActor(x: TargetActor): State = copy(all = all.removed(x))

    def withoutOldest(n: Int): (State, IterableOnce[TargetActor]) = {
      val oldest = all.toArray.sortInPlaceBy { case (_, TargetActorMeta(index, _)) => index }.take(n).map(_._1)
      (copy(all = all -- oldest), oldest)
    }
  }
}
