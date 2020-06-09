package com.wavesplatform.dex.api.websockets.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.WsOrdersUpdate
import com.wavesplatform.dex.error.ErrorFormatterContext
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.settings.SubscriptionsSettings

import scala.concurrent.duration._

/**
  * Contains subscriptions for
  * Handles user messages (pongs) add schedules timeouts (pongs, max connection lifetime)
  */
object WsInternalHandlerDirectoryActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class Subscribe(clientRef: ActorRef[WsInternalHandlerActor.Message]) extends Command
    case class CollectMatch(event: ExchangeTransactionCreated)                extends Command
    case class CollectCancel()                                                extends Command
    private[WsInternalHandlerDirectoryActor] case object SendWsUpdates        extends Command
  }

  final case class Settings(maxConnectionLifetime: FiniteDuration, subscriptions: SubscriptionsSettings)

  // TODO spawn?
  // TODO accept cancels
  // TODO batches
  // TODO ignore on start
  def apply()(implicit efc: ErrorFormatterContext): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.system.eventStream

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case Command.Subscribe(clientRef) =>
              context.watch(clientRef)
              default(state.copy(subscriptions = state.subscriptions + clientRef))

            case Command.CollectMatch(event) =>
              val x       = WsOrdersUpdate.from(event)
              val updated = state.collectedUpdates.map(_.append(x))
              default(state.copy(collectedUpdates = updated))
          }
          .receiveSignal {
            case (_, Terminated(ws)) => default(state.copy(subscriptions = state.subscriptions - ws.unsafeUpcast[WsInternalHandlerActor.Message]))
          }

      default(State(none, Set.empty))
    }

  private case class State(collectedUpdates: Option[WsOrdersUpdate], subscriptions: Set[ActorRef[WsInternalHandlerActor.Message]])
}
