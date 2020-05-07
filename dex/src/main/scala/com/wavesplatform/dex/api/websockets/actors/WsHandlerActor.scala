package com.wavesplatform.dex.api.websockets.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.api.websockets.actors.WsHandlerActor.Command.ProcessAssetPairValidationResult
import com.wavesplatform.dex.domain.account.{Address, AddressScheme}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.error.{MatcherError, WsConnectionMaxLifetimeExceeded, WsConnectionPongTimeout}
import com.wavesplatform.dex.market.{AggregatedOrderBookActor, MatcherActor}
import com.wavesplatform.dex.time.Time
import com.wavesplatform.dex.{AddressActor, AddressDirectory, AssetPairBuilder, error}
import shapeless.{Inl, Inr}

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object WsHandlerActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class ProcessClientMessage(wsMessage: WsClientMessage)                                                               extends Command
    case class ProcessAssetPairValidationResult(assetPair: AssetPair, validationResult: Try[Either[MatcherError, AssetPair]]) extends Command
    case class CloseConnection(reason: MatcherError)                                                                          extends Command
    private[WsHandlerActor] case object SendPing                                                                              extends Command
  }

  case class Completed(completionStatus: Either[Throwable, Unit]) extends Message // Could be an event in the future

  final case class Settings(maxConnectionLifetime: FiniteDuration, pingInterval: FiniteDuration, pongTimeout: FiniteDuration, jwtPublicKey: String)

  def apply(settings: Settings,
            time: Time,
            assetPairBuilder: AssetPairBuilder,
            clientRef: ActorRef[WsServerMessage],
            matcherRef: classic.ActorRef,
            addressRef: classic.ActorRef): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      context.setLoggerName(s"WsHandlerActor[c=${clientRef.path.name}]")

      def scheduleOnce(delay: FiniteDuration, message: Message): Cancellable = context.scheduleOnce(delay, context.self, message)

      val maxLifetimeExceeded = scheduleOnce(settings.maxConnectionLifetime, Command.CloseConnection(WsConnectionMaxLifetimeExceeded))
      val firstPing           = scheduleOnce(settings.pingInterval, Command.SendPing)

      def schedulePongTimeout(): Cancellable = scheduleOnce(settings.pongTimeout, Command.CloseConnection(WsConnectionPongTimeout))

      def sendPingAndScheduleNextOne(): (WsPingOrPong, Cancellable) = {
        val ping     = WsPingOrPong(time.getTimestamp())
        val nextPing = scheduleOnce(settings.pingInterval, Command.SendPing)
        clientRef ! ping
        ping -> nextPing
      }

      def cancelSchedules(nextPing: Cancellable, pongTimeout: Cancellable): Unit =
        List(nextPing, pongTimeout, maxLifetimeExceeded, firstPing).foreach { _.cancel() }

      def unsubscribeAddress(address: Address): Unit = {
        context.log.debug(s"WsUnsubscribe(address=$address)")
        addressRef ! AddressDirectory.Envelope(address, AddressActor.WsCommand.RemoveWsSubscription(clientRef))
      }

      def unsubscribeOrderBook(assetPair: AssetPair): Unit = {
        context.log.debug(s"WsUnsubscribe(assetPair=$assetPair)")
        matcherRef ! MatcherActor.AggregatedOrderBookEnvelope(assetPair, AggregatedOrderBookActor.Command.RemoveWsSubscription(clientRef))
      }

      def unsubscribe(key: WsUnsubscribe.Key): Unit = key match {
        case Inl(x)      => unsubscribeOrderBook(x)
        case Inr(Inl(x)) => unsubscribeAddress(x)
        case Inr(Inr(_)) =>
      }

      def awaitPong(maybeExpectedPong: Option[WsPingOrPong],
                    pongTimeout: Cancellable,
                    nextPing: Cancellable,
                    orderBookSubscriptions: List[AssetPair],
                    addressSubscriptions: List[Address]): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {

            case Command.SendPing =>
              val (expectedPong, newNextPing)     = sendPingAndScheduleNextOne()
              val updatedPongTimeout: Cancellable = if (pongTimeout.isCancelled) schedulePongTimeout() else pongTimeout
              awaitPong(expectedPong.some, updatedPongTimeout, newNextPing, orderBookSubscriptions, addressSubscriptions)

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
                        awaitPong(none, Cancellable.alreadyCancelled, nextPing, orderBookSubscriptions, addressSubscriptions)
                      } else {
                        context.log.trace("Got outdated pong: {}", pong)
                        Behaviors.same
                      }
                  }

                case subscribe: WsOrderBookSubscribe =>
                  // TODO DEX-700 test for order book that hasn't been created before
                  if (subscribe.depth <= 0) clientRef ! WsError.from(error.RequestArgumentInvalid("depth"), time.getTimestamp())
                  else
                    context.pipeToSelf(assetPairBuilder.validateAssetPair(subscribe.key).value) { ProcessAssetPairValidationResult(subscribe.key, _) }
                  Behaviors.same

                case subscribe: WsAddressSubscribe =>
                  subscribe.validate(settings.jwtPublicKey, AddressScheme.current.chainId) match {
                    case Left(e) =>
                      context.log.debug(s"WsAddressSubscribe(k=${subscribe.key}, t=${subscribe.authType}) failed with ${e.message.text}")
                      clientRef ! WsError.from(e, time.getTimestamp())
                      Behaviors.same
                    case Right(_) =>
                      context.log.debug(s"WsAddressSubscribe(k=${subscribe.key}, t=${subscribe.authType}) is successful")
                      addressRef ! AddressDirectory.Envelope(subscribe.key, AddressActor.WsCommand.AddWsSubscription(clientRef))
                      awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, subscribe.key :: addressSubscriptions)
                  }

                case unsubscribeRequest: WsUnsubscribe =>
                  unsubscribe(unsubscribeRequest.key)
                  Behaviors.same
              }

            case ProcessAssetPairValidationResult(assetPair, validationResult) =>
              validationResult match {
                case Success(Left(e)) => clientRef ! WsError.from(e, time.getTimestamp()); Behaviors.same
                case Success(Right(_)) =>
                  matcherRef ! MatcherActor.AggregatedOrderBookEnvelope(
                    assetPair,
                    AggregatedOrderBookActor.Command.AddWsSubscription(clientRef)
                  )
                  awaitPong(maybeExpectedPong, pongTimeout, nextPing, assetPair :: orderBookSubscriptions, addressSubscriptions)
                case Failure(e) =>
                  context.log.warn(s"An error during validation the asset pair $assetPair", e)
                  clientRef ! WsError.from(error.WavesNodeConnectionBroken, time.getTimestamp())
                  Behaviors.same
              }

            case command: Command.CloseConnection =>
              context.log.trace("Got CloseConnection: {}", command.reason.message.text)
              clientRef ! WsError.from(command.reason, time.getTimestamp())
              clientRef ! WsServerMessage.Complete
              cancelSchedules(nextPing, pongTimeout)
              Behaviors.same // Will receive Completed when WsServerMessage.Complete will be delivered

            case Completed(status) =>
              status match {
                case Left(e)  => context.log.debug("Got failure Completed. Stopping...", e)
                case Right(_) => context.log.debug("Got successful Completed. Stopping...")
              }

              addressSubscriptions foreach unsubscribeAddress
              orderBookSubscriptions foreach unsubscribeOrderBook

              cancelSchedules(nextPing, pongTimeout)
              Behaviors.stopped
          }

      awaitPong(
        maybeExpectedPong = None,
        pongTimeout = Cancellable.alreadyCancelled,
        nextPing = Cancellable.alreadyCancelled,
        orderBookSubscriptions = List.empty,
        addressSubscriptions = List.empty
      )
    }
}
