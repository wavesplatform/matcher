package com.wavesplatform.dex.api.websockets.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.api.websockets.actors.WsInternalHandlerDirectoryActor.Command.Subscribe
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
    case class Subscribe(clientRef: ActorRef[WsInternalHandlerActor.Message])         extends Command
    case class ForwardClientsMessage(wsServerMessage: WsServerMessage) extends Command
  }

  final case class Settings(maxConnectionLifetime: FiniteDuration, subscriptions: SubscriptionsSettings)

  // TODO spawn?
  def apply(settings: Settings): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      def state(subscriptions: Set[ActorRef[WsInternalHandlerActor.Message]]): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case Subscribe(clientRef) =>
              context.watch(clientRef)
              state(subscriptions + clientRef)

            case Command.ForwardClientsMessage(wsMessage) =>
              val translated = WsInternalHandlerActor.Command.ForwardClientMessage(wsMessage)
              subscriptions.foreach(_ ! translated)
              Behaviors.same
          }
          .receiveSignal {
            case (_, Terminated(ws)) => state(subscriptions - ws.unsafeUpcast[WsInternalHandlerActor.Message])
          }

      state(Set.empty)
    }
}
