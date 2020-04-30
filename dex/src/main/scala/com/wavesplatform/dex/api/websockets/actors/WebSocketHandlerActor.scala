package com.wavesplatform.dex.api.websockets.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.error.{MatcherError, WsConnectionMaxLifetimeExceeded, WsConnectionPongTimeout}
import com.wavesplatform.dex.market.{AggregatedOrderBookActor, MatcherActor}
import com.wavesplatform.dex.{AddressActor, AddressDirectory}
import shapeless.{Inl, Inr}

import scala.concurrent.duration._

object WebSocketHandlerActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class ProcessClientMessage(wsMessage: WsClientMessage) extends Command
    case class ProcessClientError(error: Throwable)             extends Command
    case object Stop                                            extends Command

    private[WebSocketHandlerActor] case object SendPing                             extends Command
    private[WebSocketHandlerActor] case class CloseConnection(reason: MatcherError) extends Command
  }

  final case class Settings(pingInterval: FiniteDuration, pongTimeout: FiniteDuration)

  // TODO maxConnectionLifetime
  def apply(settings: Settings,
            maxConnectionLifetime: FiniteDuration,
            clientRef: ActorRef[WsMessage],
            matcherRef: classic.ActorRef,
            addressRef: classic.ActorRef): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.setLoggerName(s"WebSocketHandlerActor[c=${clientRef.path.name}]")

      def scheduleOnce(delay: FiniteDuration, message: Message): Cancellable = context.scheduleOnce(delay, context.self, message)

      val maxLifetimeExceeded = scheduleOnce(maxConnectionLifetime, Command.CloseConnection(WsConnectionMaxLifetimeExceeded))
      val firstPing           = scheduleOnce(settings.pingInterval, Command.SendPing)

      def schedulePongTimeout(): Cancellable = scheduleOnce(settings.pongTimeout, Command.CloseConnection(WsConnectionPongTimeout))

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

          case Command.ProcessClientMessage(wsMessage) =>
            wsMessage match {
              case pong: WsPingOrPong =>
                maybeExpectedPong match {
                  case None =>
                    context.log.trace("Got unexpected pong: {}", pong)
                    Behaviors.same

                  case Some(expectedPong) =>
                    if (pong == expectedPong) {
                      pongTimeout.cancel()
                      awaitPong(none, Cancellable.alreadyCancelled, nextPing)
                    } else {
                      context.log.trace("Got outdated pong: {}", pong)
                      Behaviors.same
                    }
                }

              case subscribe: WsOrderBookSubscribe =>
                /*
                private def withAssetPair(p: AssetPair): Directive1[AssetPair] = {
    FutureDirectives.onSuccess { assetPairBuilder.validateAssetPair(p).value } flatMap {
      case Right(_) => provide(p)
      case Left(e)  => complete { e.toWsHttpResponse(StatusCodes.BadRequest) }
    }
  }

  private def unavailableOrderBookBarrier(p: AssetPair): Directive0 = orderBook(p) match {
    case Some(x) => if (x.isRight) pass else complete(error.OrderBookBroken(p).toWsHttpResponse(StatusCodes.ServiceUnavailable))
    case None    => complete(error.OrderBookStopped(p).toWsHttpResponse(StatusCodes.NotFound))
  }
                 */
                matcherRef ! MatcherActor.AggregatedOrderBookEnvelope(subscribe.key, AggregatedOrderBookActor.Command.AddWsSubscription(clientRef))
                Behaviors.same

              case subscribe: WsAddressSubscribe =>
                addressRef ! AddressDirectory.Envelope(subscribe.key, AddressActor.WsCommand.AddWsSubscription(clientRef))
                Behaviors.same

              case unsubscribe: WsUnsubscribe =>
                unsubscribe.key match {
                  case Inl(x) =>
                    matcherRef ! MatcherActor.AggregatedOrderBookEnvelope(x, AggregatedOrderBookActor.Command.RemoveWsSubscription(clientRef))
                  case Inr(Inl(x)) =>
                    addressRef ! AddressDirectory.Envelope(x, AddressActor.WsCommand.RemoveWsSubscription(clientRef))
                  case Inr(Inr(_)) =>
                }
                Behaviors.same
            }

          case command: Command.ProcessClientError =>
            context.log.debug("Got an error from the client, stopping: {}", command.error)
            Behaviors.stopped

          case Command.Stop => // TODO ???
            context.log.debug("Got a stop request, stopping...")
            Behaviors.stopped

          case command: Command.CloseConnection =>
            List(nextPing, pongTimeout, maxLifetimeExceeded, firstPing).foreach { _.cancel() }
            context.log.trace("Got a close request, stopping: {}", command.reason.message.text)
            clientRef ! WsMessage.Complete
            Behaviors.stopped
        }

      awaitPong(
        maybeExpectedPong = None,
        pongTimeout = Cancellable.alreadyCancelled,
        nextPing = Cancellable.alreadyCancelled
      )
    }
}
