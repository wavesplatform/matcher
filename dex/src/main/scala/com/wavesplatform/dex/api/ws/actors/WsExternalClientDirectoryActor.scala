package com.wavesplatform.dex.api.ws.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import com.wavesplatform.dex.api.http.entities.HttpWebSocketConnections
import com.wavesplatform.dex.api.ws.protocol.WsRatesUpdates
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.error

import scala.collection.immutable.{HashMap, TreeMap}

/**
 * Contains subscriptions for internal stream and broadcast messages.
 */
object WsExternalClientDirectoryActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Subscribe(clientRef: ActorRef[WsExternalClientHandlerActor.Message], os: String, client: String) extends Command
    case class CloseOldest(number: Int) extends Command
    case class BroadcastRatesUpdates(newRates: Map[Asset, Double]) extends Command
  }

  sealed trait Query extends Message

  object Query {
    case class GetActiveNumber(client: ActorRef[HttpWebSocketConnections]) extends Query
  }

  def apply(): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.system.eventStream

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case Command.Subscribe(clientRef, os, client) =>
              context.watch(clientRef)
              default(state.withActor(clientRef, os, client))

            case Command.CloseOldest(n) =>
              val (updatedState, oldest) = state.withoutOldest(n)
              oldest.iterator.foreach(_ ! WsExternalClientHandlerActor.Command.CloseConnection(error.Balancing))
              default(updatedState)

            case Command.BroadcastRatesUpdates(newRates) =>
              state.all.foreach {
                case (target, _) =>
                  target ! WsExternalClientHandlerActor.Command.ForwardToClient(WsRatesUpdates.broadcastUpdates(newRates))
              }
              default(state)

            case Query.GetActiveNumber(client) =>
              client ! HttpWebSocketConnections(state.all.size, state.infoMap)
              default(state)
          }
          .receiveSignal {
            case (_, Terminated(clientRef)) =>
              default {
                state.withoutActor(clientRef.unsafeUpcast: TargetActor)
              }
          }

      default(State(0, HashMap.empty, TreeMap.empty))
    }

  private type TargetActor = ActorRef[WsExternalClientHandlerActor.Message]
  private type Index = Int

  private case class State(currentIndex: Index, all: Map[TargetActor, ConnectionInfo], infoMap: Map[String, Int]) {

    def withActor(x: TargetActor, os: String, client: String): State = {
      val info = ConnectionInfo(currentIndex, os, client)
      copy(
        currentIndex = currentIndex + 1,
        all = all.updated(x, info),
        infoMap = infoMap.updated(info.clientAndOs, infoMap.getOrElse(info.clientAndOs, 0) + 1)
      )
    }

    def withoutActor(x: TargetActor): State = all.get(x) match {
      case None => this
      case Some(info) =>
        val updatedNumber = infoMap.getOrElse(info.clientAndOs, 1) - 1
        val updatedInfoMap =
          if (updatedNumber == 0) infoMap.removed(info.clientAndOs)
          else infoMap.updated(info.clientAndOs, updatedNumber)
        copy(
          all = all.removed(x),
          infoMap = updatedInfoMap
        )
    }

    def withoutOldest(n: Int): (State, IterableOnce[TargetActor]) = {
      val oldest = all.toArray.sortInPlaceBy { case (_, info) => info.index }.take(n)

      val infoDiff = oldest.groupMapReduce(_._2.clientAndOs)(_ => 1)(_ + _)
      val updatedInfoMap = infoDiff.foldLeft(infoMap) {
        case (r, (k, v)) =>
          r.get(k) match {
            case None => r
            case Some(origV) => r.updated(k, math.max(0, origV - v))
          }
      }

      val oldestActors = oldest.map(_._1)
      (copy(all = all -- oldestActors, infoMap = updatedInfoMap), oldestActors)
    }

  }

  private case class ConnectionInfo(index: Index, os: String, client: String) {
    val clientAndOs = s"$client, $os"
  }

}
