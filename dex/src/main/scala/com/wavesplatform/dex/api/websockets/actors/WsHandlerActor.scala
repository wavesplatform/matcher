package com.wavesplatform.dex.api.websockets.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.api.websockets._
import com.wavesplatform.dex.api.websockets.actors.WsHandlerActor.Command.{AssetPairValidated, CancelAddressSubscription}
import com.wavesplatform.dex.domain.account.{Address, AddressScheme}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.error.{MatcherError, WsConnectionMaxLifetimeExceeded, WsConnectionPongTimeout}
import com.wavesplatform.dex.market.{AggregatedOrderBookActor, MatcherActor}
import com.wavesplatform.dex.time.Time
import com.wavesplatform.dex.{AddressActor, AddressDirectory, AssetPairBuilder, error}
import shapeless.{Inl, Inr}

import scala.concurrent.duration._
import scala.util.{Failure, Success}

object WsHandlerActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message
  object Command {
    case class ProcessClientMessage(wsMessage: WsClientMessage) extends Command
    case class ForwardClientError(error: MatcherError)          extends Command
    case class CloseConnection(reason: MatcherError)            extends Command
    case class CancelAddressSubscription(address: Address)      extends Command

    private[WsHandlerActor] case class AssetPairValidated(assetPair: AssetPair) extends Command
    private[WsHandlerActor] case object SendPing                                extends Command
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
      import context.executionContext

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

      def awaitPong(maybeExpectedPong: Option[WsPingOrPong],
                    pongTimeout: Cancellable,
                    nextPing: Cancellable,
                    orderBookSubscriptions: List[AssetPair],
                    addressSubscriptions: Map[Address, Cancellable]): Behavior[Message] =
        Behaviors
          .receiveMessage[Message] {

            case Command.SendPing =>
              val (expectedPong, newNextPing)     = sendPingAndScheduleNextOne()
              val updatedPongTimeout: Cancellable = if (pongTimeout.isCancelled) schedulePongTimeout() else pongTimeout
              awaitPong(expectedPong.some, updatedPongTimeout, newNextPing, orderBookSubscriptions, addressSubscriptions)

            case Command.ForwardClientError(matcherError) =>
              clientRef ! WsError.from(matcherError, time.correctedTime())
              Behaviors.same

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
                    assetPairBuilder.validateAssetPair(subscribe.key).value.onComplete {
                      case Success(Left(e))  => clientRef ! WsError.from(e, time.getTimestamp())
                      case Success(Right(_)) => context.self ! AssetPairValidated(subscribe.key)
                      case Failure(e) =>
                        context.log.warn(s"An error during validation the asset pair ${subscribe.key}", e)
                        clientRef ! WsError.from(error.WavesNodeConnectionBroken, time.getTimestamp())
                    }
                  Behaviors.same

                case subscribe: WsAddressSubscribe =>
                  val address  = subscribe.key
                  val authType = subscribe.authType
                  subscribe.validate(settings.jwtPublicKey, AddressScheme.current.chainId) match {
                    case Left(e) =>
                      context.log.debug(s"WsAddressSubscribe(k=$address, t=$authType) failed with ${e.message.text}")
                      clientRef ! WsError.from(e, time.getTimestamp())
                      Behaviors.same
                    case Right(jwtPayload) =>
                      val subscriptionLifetime = (jwtPayload.activeTokenExpirationInSeconds * 1000 - time.correctedTime).millis

                      addressSubscriptions
                        .get(subscribe.key)
                        .fold {
                          addressRef ! AddressDirectory.Envelope(subscribe.key, AddressActor.WsCommand.AddWsSubscription(clientRef))
                          context.log.debug(s"WsAddressSubscribe(k=$address, t=$authType) is successful, will expire in $subscriptionLifetime")
                        } { existedExp =>
                          existedExp.cancel()
                          context.log.debug(s"WsAddressSubscribe(k=$address, t=$authType) updated, will expire in $subscriptionLifetime")
                        }

                      val expiration = scheduleOnce(subscriptionLifetime, CancelAddressSubscription(address))
                      awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, addressSubscriptions + (address -> expiration))
                  }

                case unsubscribeRequest: WsUnsubscribe =>
                  unsubscribeRequest.key match {
                    case Inl(ap) =>
                      orderBookSubscriptions.find(_ == ap).fold[Behavior[Message]](Behaviors.same) { assetPair =>
                        unsubscribeOrderBook(assetPair)
                        awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions.filterNot(_ == assetPair), addressSubscriptions)
                      }

                    case Inr(Inl(address)) =>
                      addressSubscriptions.get(address).fold[Behavior[Message]](Behaviors.same) { expiration =>
                        expiration.cancel()
                        unsubscribeAddress(address)
                        awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, addressSubscriptions - address)
                      }
                    case Inr(Inr(_)) => Behaviors.same
                  }
              }

            case Command.AssetPairValidated(assetPair) =>
              matcherRef ! MatcherActor.AggregatedOrderBookEnvelope(assetPair, AggregatedOrderBookActor.Command.AddWsSubscription(clientRef))
              awaitPong(maybeExpectedPong, pongTimeout, nextPing, assetPair :: orderBookSubscriptions, addressSubscriptions)

            case Command.CancelAddressSubscription(address) =>
              clientRef ! WsError.from(error.SubscriptionTokenExpired(address), time.correctedTime())
              unsubscribeAddress(address)
              awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, addressSubscriptions - address)

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

              addressSubscriptions foreach { case (address, expiration) => expiration.cancel(); unsubscribeAddress(address) }
              orderBookSubscriptions foreach unsubscribeOrderBook

              cancelSchedules(nextPing, pongTimeout)
              Behaviors.stopped
          }

      awaitPong(
        maybeExpectedPong = None,
        pongTimeout = Cancellable.alreadyCancelled,
        nextPing = Cancellable.alreadyCancelled,
        orderBookSubscriptions = List.empty,
        addressSubscriptions = Map.empty
      )
    }
}
