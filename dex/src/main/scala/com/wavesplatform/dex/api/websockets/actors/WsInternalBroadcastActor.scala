package com.wavesplatform.dex.api.websockets.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.WsOrdersUpdate
import play.api.libs.json.Json

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

  final case class Settings(messagesInterval: FiniteDuration)

  def apply(settings: Settings): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.system.eventStream

      def default(state: State): Behavior[Message] = {
        context.log.info(s"State:\nsubscriptions: ${state.subscriptions.map(_.path.name).toList.sorted}\nupdates: ${state.collectedUpdates}\nschedule.cancelled: ${state.schedule.isCancelled}")
        Behaviors
          .receiveMessage[Message] {
            case Command.Collect(update) =>
              context.log.info(s"Got Collect(${Json.toJson(update)})")
              default {
                state
                  .withUpdates(update)
                  .runSchedule(settings.messagesInterval, context)
              }

            case Command.Subscribe(clientRef) =>
              context.log.info(s"[${clientRef.path.name}] subscribed")
              context.watch(clientRef)
              default {
                state
                  .updateSubscriptions(_ + clientRef)
                  .runSchedule(settings.messagesInterval, context)
              }

            case Command.SendWsUpdates =>
              context.log.info(s"Got SendWsUpdates, sending ${Json.toJson(state.collectedUpdates)}")
              state.collectedUpdates
                .map(WsInternalClientHandlerActor.Command.ForwardToClient)
                .foreach { message =>
                  state.subscriptions.foreach(_ ! message)
                }
              default(state.withoutUpdates.withCompletedSchedule.runSchedule(settings.messagesInterval, context))
          }
          .receiveSignal {
            case (_, Terminated(clientRef)) =>
              context.log.info(s"[${clientRef.path.name}] unsubscribed")
              default(state.updateSubscriptions(_ - clientRef.unsafeUpcast[WsInternalClientHandlerActor.Message]))
          }
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

    // Cancellable.isCancelled == false if the task was completed
    def withCompletedSchedule: State = copy(schedule = Cancellable.alreadyCancelled)

    def runSchedule(interval: FiniteDuration, context: ActorContext[Message]): State =
      if (schedule.isCancelled && subscriptions.nonEmpty && collectedUpdates.nonEmpty)
        copy(schedule = context.scheduleOnce(interval, context.self, Command.SendWsUpdates))
      else this

    def withoutUpdates: State = copy(collectedUpdates = none)
    def withUpdates(x: WsOrdersUpdate): State =
      if (subscriptions.isEmpty) this
      else
        copy(
          collectedUpdates = collectedUpdates
            .foldLeft(x) { case (updates, orig) => orig.append(updates) }
            .some
        )
  }
}
