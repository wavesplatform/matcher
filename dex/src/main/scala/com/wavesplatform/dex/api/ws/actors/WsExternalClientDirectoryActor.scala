package com.wavesplatform.dex.api.ws.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import com.wavesplatform.dex.error

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
  }

  sealed trait Query extends Message
  object Query {
    case class GetActiveNumber(client: ActorRef[Int]) extends Query
  }

  def apply(): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.system.eventStream

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case Command.Subscribe(clientRef) =>
              context.watch(clientRef)
              default {
                state.withActor(clientRef)
              }

            case Command.CloseOldest(n) =>
              val (updatedState, oldest) = state.withoutOldest(n)
              oldest.iterator.foreach(_ ! WsExternalClientHandlerActor.Command.CloseConnection(error.Balancing))
              default(updatedState)

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
  private case class State(currentIndex: Index, all: HashMap[TargetActor, Index]) {
    def withActor(x: TargetActor): State = copy(
      currentIndex = currentIndex + 1,
      all = all.updated(x, currentIndex)
    )

    def withoutActor(x: TargetActor): State = copy(all = all.removed(x))
    def withoutOldest(n: Int): (State, IterableOnce[TargetActor]) = {
      val oldest = all.toArray.sortInPlaceBy(_._2).take(n).map(_._1)
      (copy(all = all -- oldest), oldest)
    }
  }
}
