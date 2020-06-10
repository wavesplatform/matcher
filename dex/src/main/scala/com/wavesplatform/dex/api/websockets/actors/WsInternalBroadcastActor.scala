package com.wavesplatform.dex.api.websockets.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.WsOrdersUpdate

import scala.concurrent.duration._

/**
  * Contains subscriptions for
  * Handles user messages (pongs) add schedules timeouts (pongs, max connection lifetime)
  */
object WsInternalBroadcastActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class Subscribe(clientRef: ActorRef[WsInternalClientHandlerActor.Message]) extends Command
    private[WsInternalBroadcastActor] case object SendWsUpdates                     extends Command

    case class Collect(update: WsOrdersUpdate) extends Command
  }

  final case class Settings(wsMessagesInterval: FiniteDuration)

  def apply(settings: Settings): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.system.eventStream

      def default(state: State): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {
            case Command.Collect(update) =>
              default {
                state
                  .withUpdates(update)
                  .runSchedule(settings.wsMessagesInterval, context)
              }

            case Command.Subscribe(clientRef) =>
              context.watch(clientRef)
              default {
                state
                  .updateSubscriptions(_ + clientRef)
                  .runSchedule(settings.wsMessagesInterval, context)
              }

            case Command.SendWsUpdates =>
              state.collectedUpdates
                .map(WsInternalClientHandlerActor.Command.ForwardToClient)
                .foreach { message =>
                  state.subscriptions.foreach(_ ! message)
                }
              default(state.withoutUpdates.runSchedule(settings.wsMessagesInterval, context))
          }
          .receiveSignal {
            case (_, Terminated(ws)) => default(state.updateSubscriptions(_ - ws.unsafeUpcast[WsInternalClientHandlerActor.Message]))
          }

      default(State(none, Set.empty, Cancellable.alreadyCancelled))
    }

  private type Subscriptions = Set[ActorRef[WsInternalClientHandlerActor.Message]]

  private case class State(collectedUpdates: Option[WsOrdersUpdate], subscriptions: Subscriptions, schedule: Cancellable) {
    def updateSubscriptions(f: Subscriptions => Subscriptions): State = {
      val updated = f(subscriptions)
      val s       = copy(subscriptions = updated)
      if (updated.isEmpty) {
        schedule.cancel()
        s.withoutUpdates
      } else s
    }

    def runSchedule(interval: FiniteDuration, context: ActorContext[Message]): State =
      if (schedule.isCancelled && subscriptions.nonEmpty) copy(schedule = context.scheduleOnce(interval, context.self, Command.SendWsUpdates))
      else this

    def withoutUpdates: State                 = copy(collectedUpdates = none)
    def withUpdates(x: WsOrdersUpdate): State = if (subscriptions.isEmpty) this else copy(collectedUpdates = collectedUpdates.map(_.append(x)))
  }
}
