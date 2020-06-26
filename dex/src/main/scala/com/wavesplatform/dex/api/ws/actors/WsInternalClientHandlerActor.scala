package com.wavesplatform.dex.api.ws.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.api.ws.protocol.{WsError, WsInitial, WsPingOrPong, WsServerMessage}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.error.{MatcherError, WsConnectionPongTimeout}
import com.wavesplatform.dex.model.AssetPairBuilder
import com.wavesplatform.dex.time.Time

import scala.collection.immutable.Queue
import scala.concurrent.duration.FiniteDuration

/**
  * Controls a single WebSocket connection for order updates stream
  * Handles user messages (pongs) add schedules timeouts (pongs, max connection lifetime)
  */
object WsInternalClientHandlerActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class ProcessClientMessage(wsMessage: WsPingOrPong)     extends Command
    case class ForwardToClient(wsServerMessage: WsServerMessage) extends Command
    case class CloseConnection(reason: MatcherError)             extends Command

    private[WsInternalClientHandlerActor] case object SendPing extends Command
  }

  sealed trait Event extends Message
  object Event {
    case class Completed(completionStatus: Either[Throwable, Unit]) extends Event
  }

  final case class Settings(healthCheck: WsHealthCheckSettings)

  def apply(settings: Settings,
            time: Time,
            assetPairBuilder: AssetPairBuilder,
            clientRef: ActorRef[WsServerMessage],
            matcherRef: classic.ActorRef,
            addressRef: classic.ActorRef,
            connectionId: String): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.setLoggerName(s"WsInternalHandlerActor[c=${clientRef.path.name}]")

      def matcherTime: Long = time.getTimestamp()

      def scheduleOnce(delay: FiniteDuration, message: Message): Cancellable = context.scheduleOnce(delay, context.self, message)

      val firstPing = scheduleOnce(settings.healthCheck.pingInterval, Command.SendPing)

      def schedulePongTimeout(): Cancellable = scheduleOnce(settings.healthCheck.pongTimeout, Command.CloseConnection(WsConnectionPongTimeout))

      def sendPingAndScheduleNextOne(): (WsPingOrPong, Cancellable) = {
        val ping     = WsPingOrPong(matcherTime)
        val nextPing = scheduleOnce(settings.healthCheck.pingInterval, Command.SendPing)
        clientRef ! ping
        ping -> nextPing
      }

      def cancelSchedules(nextPing: Cancellable, pongTimeout: Cancellable): Unit =
        List(nextPing, pongTimeout, firstPing).foreach { _.cancel() }

      def awaitPong(maybeExpectedPong: Option[WsPingOrPong],
                    pongTimeout: Cancellable,
                    nextPing: Cancellable,
                    orderBookSubscriptions: Queue[AssetPair],
                    addressSubscriptions: Queue[(Address, Cancellable)]): Behavior[Message] = {
        Behaviors
          .receiveMessage[Message] {
            case Command.SendPing =>
              val (expectedPong, newNextPing) = sendPingAndScheduleNextOne()
              val updatedPongTimeout          = if (pongTimeout.isCancelled) schedulePongTimeout() else pongTimeout
              awaitPong(expectedPong.some, updatedPongTimeout, newNextPing, orderBookSubscriptions, addressSubscriptions)

            case Command.ForwardToClient(wsMessage) =>
              clientRef ! wsMessage
              Behaviors.same

            case Command.ProcessClientMessage(pong) =>
              maybeExpectedPong match {
                case None =>
                  context.log.trace("Got unexpected pong: {}", pong)
                  Behaviors.same

                case Some(expectedPong) =>
                  if (pong == expectedPong) {
                    pongTimeout.cancel()
                    awaitPong(none, Cancellable.alreadyCancelled, nextPing, orderBookSubscriptions, addressSubscriptions)
                  } else {
                    context.log.trace("Got outdated pong: {}", pong)
                    Behaviors.same
                  }
              }

            case command: Command.CloseConnection =>
              context.log.trace("Got CloseConnection: {}", command.reason.message.text)
              clientRef ! WsError.from(command.reason, matcherTime)
              clientRef ! WsServerMessage.Complete
              cancelSchedules(nextPing, pongTimeout)
              Behaviors.same // Will receive Completed when WsServerMessage.Complete will be delivered

            case Event.Completed(status) =>
              status match {
                case Left(e)  => context.log.debug("Got failure Completed. Stopping...", e)
                case Right(_) => context.log.debug("Got successful Completed. Stopping...")
              }

              cancelSchedules(nextPing, pongTimeout)
              Behaviors.stopped
          }
      }

      // send the initial message with the connection ID for further debugging
      clientRef ! WsInitial(connectionId, matcherTime)

      awaitPong(
        maybeExpectedPong = None,
        pongTimeout = Cancellable.alreadyCancelled,
        nextPing = Cancellable.alreadyCancelled,
        orderBookSubscriptions = Queue.empty,
        addressSubscriptions = Queue.empty
      )
    }
}
