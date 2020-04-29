package com.wavesplatform.dex.api.websockets.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets.{WsMessage, WsPingOrPong}
import com.wavesplatform.dex.error.{MatcherError, WsConnectionMaxLifetimeExceeded, WsConnectionPongTimeout}

import scala.concurrent.duration._

object WebSocketHandlerActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class ProcessClientMessage(wsMessage: WsPingOrPong) extends Command
    case class ProcessClientError(error: Throwable)          extends Command
    case object Stop                                         extends Command

    private[WebSocketHandlerActor] case object SendPing                             extends Command
    private[WebSocketHandlerActor] case class CloseConnection(reason: MatcherError) extends Command
  }

  final case class Settings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)

  // TODO maxConnectionLifetime
  def apply(settings: Settings, maxConnectionLifetime: FiniteDuration, clientRef: ActorRef[WsMessage]): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      def scheduleOnce(delay: FiniteDuration, message: Message): Cancellable = context.scheduleOnce(delay, context.self, message)

      val maxLifetimeExceeded = scheduleOnce(maxConnectionLifetime, Command.CloseConnection(WsConnectionMaxLifetimeExceeded))
      val firstPing           = scheduleOnce(settings.pingInterval, Command.SendPing)

      def schedulePongTimeout(): Cancellable = scheduleOnce(settings.pongTimeout, Command.CloseConnection(WsConnectionPongTimeout))

      def closeSourceAndCancelAllTasks(reason: MatcherError, tasks: List[Cancellable]): Behavior[Message] = {
        tasks.foreach { _.cancel() }
        context.log.trace(s"[${clientRef.path.name}] ${reason.message.text}")
        clientRef ! WsMessage.Complete
        Behaviors.stopped[Message]
      }

      def sendPingAndScheduleNextOne(): (WsPingOrPong, Cancellable) = {
        val ping     = WsPingOrPong(System.currentTimeMillis)
        val nextPing = scheduleOnce(settings.pingInterval, Command.SendPing)
        clientRef ! ping
        ping -> nextPing
      }

      def awaitPong(maybeExpectedPong: Option[WsPingOrPong], pongTimeout: Cancellable, nextPing: Cancellable): Behavior[Message] =
        Behaviors.receiveMessage[Message] {
          case Command.SendPing =>
            val (expectedPong, newNextPing)     = sendPingAndScheduleNextOne()
            val updatedPongTimeout: Cancellable = if (pongTimeout.isCancelled) schedulePongTimeout() else pongTimeout
            awaitPong(expectedPong.some, updatedPongTimeout, newNextPing)

          case Command.ProcessClientMessage(pong: WsPingOrPong) =>
            maybeExpectedPong match {
              case None =>
                context.log.trace("Got unexpected pong: {}", pong)
                Behaviors.same

              case Some(expectedPong) =>
                if (pong == expectedPong) {
                  pongTimeout.cancel()
                  awaitPong(none, Cancellable.alreadyCancelled, nextPing)
                } else {
                  context.log.trace("Got outdated pong", pong)
                  Behaviors.same
                }
            }

          case Command.CloseConnection(reason) =>
            closeSourceAndCancelAllTasks(reason, List(nextPing, pongTimeout, maxLifetimeExceeded, firstPing))

          case Command.ProcessClientError(e) =>
            context.log.debug("Got an error from the client, stopping...", e)
            Behaviors.stopped

          case Command.Stop =>
            context.log.debug("Got a stop request, stopping...")
            Behaviors.stopped
        }

      Behaviors
        .receiveMessage[Message] {
          case Command.SendPing =>
            val (expectedPong, nextPing) = sendPingAndScheduleNextOne()
            val pongTimeout: Cancellable = schedulePongTimeout()
            awaitPong(expectedPong.some, pongTimeout, nextPing)

          case Command.CloseConnection(reason) =>
            closeSourceAndCancelAllTasks(reason, List(maxLifetimeExceeded, firstPing))

          case command: Command.ProcessClientMessage =>
            context.log.trace("Got an unexpected command: {}", command)
            Behaviors.same

          case Command.ProcessClientError(e) =>
            context.log.debug("Got an error from the client, stopping...", e)
            Behaviors.stopped

          case Command.Stop =>
            context.log.debug("Got a stop request, stopping...")
            Behaviors.stopped
        }
    }
}
