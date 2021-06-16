package com.wavesplatform.dex.api.ws.actors

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior, Terminated}
import akka.{actor => classic}
import cats.implicits.catsSyntaxEitherId
import cats.syntax.option._
import com.wavesplatform.dex.actors.OrderBookDirectoryActor
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.orderbook.AggregatedOrderBookActor
import com.wavesplatform.dex.api.ws.actors.WsExternalClientHandlerActor.Command.CancelAddressSubscription
import com.wavesplatform.dex.api.ws.protocol._
import com.wavesplatform.dex.api.ws.state.WsAddressState
import com.wavesplatform.dex.domain.account.{Address, AddressScheme}
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.error
import com.wavesplatform.dex.error.{MatcherError, SubscriptionsLimitReached, WsConnectionMaxLifetimeExceeded, WsConnectionPongTimeout}
import com.wavesplatform.dex.model.AssetPairBuilder
import com.wavesplatform.dex.settings.SubscriptionsSettings
import com.wavesplatform.dex.time.Time
import shapeless.{Inl, Inr}

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success}

/**
 * Controls WebSocket connection and order book/address subscriptions.
 * Handles user messages (pongs, subscription requests) add schedules timeouts (pongs, max connection lifetime)
 */
object WsExternalClientHandlerActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class ProcessClientMessage(wsMessage: WsClientMessage) extends Command
    case class ForwardToClient(wsMessage: WsServerMessage) extends Command
    case class CloseConnection(reason: MatcherError) extends Command

    private[WsExternalClientHandlerActor] case class CancelAddressSubscription(address: Address) extends Command
    private[WsExternalClientHandlerActor] case object SendPing extends Command
  }

  sealed trait Event extends Message

  object Event {
    private[WsExternalClientHandlerActor] case class AssetPairValidated(assetPair: AssetPair) extends Event
    case class Completed(completionStatus: Either[Throwable, Unit]) extends Event
  }

  final case class Settings(
    messagesInterval: FiniteDuration,
    maxConnectionLifetime: FiniteDuration,
    jwtPublicKey: String,
    subscriptions: SubscriptionsSettings,
    healthCheck: WsHealthCheckSettings
  )

  def apply(
    settings: Settings,
    time: Time,
    assetPairBuilder: AssetPairBuilder,
    clientRef: ActorRef[WsServerMessage],
    matcherRef: classic.ActorRef,
    addressRef: classic.ActorRef,
    connectionId: String,
    getRatesSnapshot: () => Map[Asset, Double]
  ): Behavior[Message] =
    Behaviors.setup[Message] { context =>
      import context.executionContext
      import settings.subscriptions._

      context.setLoggerName(s"WsExternalClientHandlerActor[c=${clientRef.path.name}]")
      context.watch(clientRef)

      def matcherTime: Long = time.getTimestamp()

      def scheduleOnce(delay: FiniteDuration, message: Message): Cancellable = context.scheduleOnce(delay, context.self, message)

      val maxLifetimeExceeded = scheduleOnce(settings.maxConnectionLifetime, Command.CloseConnection(WsConnectionMaxLifetimeExceeded))
      val firstPing = scheduleOnce(settings.healthCheck.pingInterval, Command.SendPing)

      def schedulePongTimeout(): Cancellable = scheduleOnce(settings.healthCheck.pongTimeout, Command.CloseConnection(WsConnectionPongTimeout))

      def sendPingAndScheduleNextOne(): (WsPingOrPong, Cancellable) = {
        val ping = WsPingOrPong(matcherTime)
        val nextPing = scheduleOnce(settings.healthCheck.pingInterval, Command.SendPing)
        clientRef ! ping
        ping -> nextPing
      }

      def cancelSchedules(nextPing: Cancellable, pongTimeout: Cancellable): Unit =
        List(nextPing, pongTimeout, maxLifetimeExceeded, firstPing).foreach(_.cancel())

      def unsubscribeAddress(address: Address): Unit = {
        context.log.debug(s"WsUnsubscribe(address=$address)")
        addressRef ! AddressDirectoryActor.Command.ForwardMessage(address, AddressActor.WsCommand.RemoveWsSubscription(clientRef))
      }

      def unsubscribeOrderBook(assetPair: AssetPair): Unit = {
        context.log.debug(s"WsUnsubscribe(assetPair=$assetPair)")
        matcherRef ! OrderBookDirectoryActor.AggregatedOrderBookEnvelope(
          assetPair,
          AggregatedOrderBookActor.Command.RemoveWsSubscription(clientRef)
        )
      }

      def awaitPong(
        maybeExpectedPong: Option[WsPingOrPong],
        pongTimeout: Cancellable,
        nextPing: Cancellable,
        orderBookSubscriptions: Queue[AssetPair],
        addressSubscriptions: Queue[(Address, Cancellable)],
        maybeRatesUpdateId: Option[Long]
      ): Behavior[Message] = {
        Behaviors
          .receiveMessage[Message] {

            case Command.SendPing =>
              val (expectedPong, newNextPing) = sendPingAndScheduleNextOne()
              val updatedPongTimeout: Cancellable = if (pongTimeout.isCancelled) schedulePongTimeout() else pongTimeout
              awaitPong(expectedPong.some, updatedPongTimeout, newNextPing, orderBookSubscriptions, addressSubscriptions, maybeRatesUpdateId)

            case Command.ForwardToClient(msg) =>
              msg match {
                case ru: WsRatesUpdates =>
                  if (maybeRatesUpdateId.nonEmpty) {
                    val newUpdateId = maybeRatesUpdateId.map(WsAddressState.getNextUpdateId).get
                    clientRef ! WsRatesUpdates(ru.rates, newUpdateId, matcherTime)
                    awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, addressSubscriptions, newUpdateId.some)
                  } else awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, addressSubscriptions, maybeRatesUpdateId)
                case other =>
                  clientRef ! other
                  Behaviors.same
              }

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
                        awaitPong(none, Cancellable.alreadyCancelled, nextPing, orderBookSubscriptions, addressSubscriptions, maybeRatesUpdateId)
                      } else {
                        context.log.trace("Got outdated pong: {}", pong)
                        Behaviors.same
                      }
                  }

                case subscribe: WsOrderBookSubscribe =>
                  if (subscribe.depth <= 0) clientRef ! WsError.from(error.RequestArgumentInvalid("depth"), matcherTime)
                  else if (!orderBookSubscriptions.contains(subscribe.key)) {
                    val self = context.self
                    context.log.debug(s"WsOrderBookSubscribe(k=${subscribe.key})")
                    assetPairBuilder.validateAssetPair(subscribe.key).value.onComplete {
                      case Success(Left(e)) => clientRef ! WsError.from(e, matcherTime)
                      case Success(Right(_)) => self ! Event.AssetPairValidated(subscribe.key)
                      case Failure(e) => clientRef ! WsError.from(error.WavesNodeConnectionBroken, matcherTime)
                    }
                  }
                  Behaviors.same

                case subscribe: WsAddressSubscribe =>
                  val address = subscribe.key
                  val authType = subscribe.authType

                  subscribe.validate(settings.jwtPublicKey, AddressScheme.current.chainId) match {
                    case Left(e) =>
                      context.log.debug(s"WsAddressSubscribe(k=$address, t=$authType) failed with ${e.message.text}")
                      clientRef ! WsError.from(e, matcherTime)
                      Behaviors.same
                    case Right(jwtPayload) =>
                      val subscriptionLifetime = (jwtPayload.activeTokenExpirationInSeconds * 1000 - time.correctedTime()).millis
                      val expiration = scheduleOnce(subscriptionLifetime, CancelAddressSubscription(address))

                      addressSubscriptions
                        .find(_._1 == subscribe.key)
                        .fold {
                          addressRef ! AddressDirectoryActor.Command.ForwardMessage(
                            subscribe.key,
                            AddressActor.WsCommand.AddWsSubscription(clientRef, subscribe.filters)
                          )
                          context.log.debug(s"WsAddressSubscribe(k=$address, t=$authType) is successful, will expire in $subscriptionLifetime")

                          if (addressSubscriptions.lengthCompare(maxAddressNumber) == 0) {
                            // safe since maxAddressNumber > 0
                            val ((evictedSubscription, evictedExpiration), remainingSubscriptions) = addressSubscriptions.dequeue
                            val newAddressSubscriptions = remainingSubscriptions enqueue address -> expiration
                            evictedExpiration.cancel()
                            unsubscribeAddress(evictedSubscription)
                            clientRef ! WsError.from(SubscriptionsLimitReached(maxAddressNumber, evictedSubscription.toString), matcherTime)
                            awaitPong(
                              maybeExpectedPong,
                              pongTimeout,
                              nextPing,
                              orderBookSubscriptions,
                              newAddressSubscriptions,
                              maybeRatesUpdateId
                            )
                          } else
                            awaitPong(
                              maybeExpectedPong,
                              pongTimeout,
                              nextPing,
                              orderBookSubscriptions,
                              addressSubscriptions enqueue address -> expiration,
                              maybeRatesUpdateId
                            )
                        } {
                          case (_, existedExpiration) =>
                            existedExpiration.cancel()
                            context.log.debug(s"WsAddressSubscribe(k=$address, t=$authType) updated, will expire in $subscriptionLifetime")
                            val newAddressSubscriptions = addressSubscriptions.foldLeft(Queue.empty[(Address, Cancellable)]) {
                              case (result, (a, e)) => result.enqueue(a -> (if (a == address) expiration else e))
                            }
                            awaitPong(
                              maybeExpectedPong,
                              pongTimeout,
                              nextPing,
                              orderBookSubscriptions,
                              newAddressSubscriptions,
                              maybeRatesUpdateId
                            )
                        }
                  }

                case _: WsRatesUpdatesSubscribe =>
                  if (maybeRatesUpdateId.nonEmpty) Behaviors.same
                  else {
                    clientRef ! WsRatesUpdates(getRatesSnapshot(), 0L, matcherTime)
                    awaitPong(
                      maybeExpectedPong,
                      pongTimeout,
                      nextPing,
                      orderBookSubscriptions,
                      addressSubscriptions,
                      maybeRatesUpdateId = 0L.some
                    )
                  }

                case unsubscribeRequest: WsUnsubscribe =>
                  unsubscribeRequest.key match {
                    case Inl(ap) =>
                      orderBookSubscriptions.find(_ == ap).fold[Behavior[Message]](Behaviors.same) { assetPair =>
                        unsubscribeOrderBook(assetPair)
                        awaitPong(
                          maybeExpectedPong,
                          pongTimeout,
                          nextPing,
                          orderBookSubscriptions.filterNot(_ == assetPair),
                          addressSubscriptions,
                          maybeRatesUpdateId
                        )
                      }

                    case Inr(Inl(address)) =>
                      addressSubscriptions.find(_._1 == address).fold[Behavior[Message]](Behaviors.same) {
                        case (_, expiration) =>
                          expiration.cancel()
                          unsubscribeAddress(address)
                          val newAddressSubscriptions = addressSubscriptions.filterNot(_._1 == address)
                          awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, newAddressSubscriptions, maybeRatesUpdateId)
                      }

                    case Inr(Inr(Inl(_))) => // rates updates unsubscribe
                      awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, addressSubscriptions, maybeRatesUpdateId = None)

                    case Inr(Inr(_)) => Behaviors.same
                  }

                case _ => throw new IllegalArgumentException(s"Can't process message=$wsMessage")
              }

            case Event.AssetPairValidated(assetPair) =>
              matcherRef ! OrderBookDirectoryActor.AggregatedOrderBookEnvelope(
                assetPair,
                AggregatedOrderBookActor.Command.AddWsSubscription(clientRef)
              )

              if (orderBookSubscriptions.lengthCompare(maxOrderBookNumber) == 0) {
                // safe since maxOrderBookNumber > 0
                val (evictedSubscription, remainingSubscriptions) = orderBookSubscriptions.dequeue
                val newOrderBookSubscriptions = remainingSubscriptions enqueue assetPair
                unsubscribeOrderBook(evictedSubscription)
                clientRef ! WsError.from(SubscriptionsLimitReached(maxOrderBookNumber, evictedSubscription.toString), matcherTime)
                awaitPong(maybeExpectedPong, pongTimeout, nextPing, newOrderBookSubscriptions, addressSubscriptions, maybeRatesUpdateId)
              } else
                awaitPong(
                  maybeExpectedPong,
                  pongTimeout,
                  nextPing,
                  orderBookSubscriptions enqueue assetPair,
                  addressSubscriptions,
                  maybeRatesUpdateId
                )

            case Command.CancelAddressSubscription(address) =>
              clientRef ! WsError.from(error.SubscriptionTokenExpired(address), matcherTime)
              unsubscribeAddress(address)
              val newAddressSubscriptions = addressSubscriptions.filterNot(_._1 == address)
              awaitPong(maybeExpectedPong, pongTimeout, nextPing, orderBookSubscriptions, newAddressSubscriptions, maybeRatesUpdateId)

            case command: Command.CloseConnection =>
              context.log.debug("Got CloseConnection: {}", command.reason.message.text)
              clientRef ! WsError.from(command.reason, matcherTime)
              context.scheduleOnce(100.millis, clientRef, WsServerMessage.Complete) // Otherwise a connection is closed too quickly
              cancelSchedules(nextPing, pongTimeout)
              Behaviors.same // Will receive Completed when WsServerMessage.Complete will be delivered

            case Event.Completed(status) =>
              status match {
                case Left(e) => context.log.debug("Got failure Completed. Stopping...", e)
                case Right(_) => context.log.debug("Got successful Completed. Stopping...")
              }

              addressSubscriptions foreach { case (address, expiration) => expiration.cancel(); unsubscribeAddress(address) }
              orderBookSubscriptions foreach unsubscribeOrderBook

              cancelSchedules(nextPing, pongTimeout)
              Behaviors.stopped
          }
          .receiveSignal {
            case (context, Terminated(_)) =>
              context.self ! clientActorStoppedEvent
              Behaviors.same
          }
      }

      // send the initial message with the connection ID for further debugging
      clientRef ! WsInitial(connectionId, matcherTime)

      awaitPong(
        maybeExpectedPong = None,
        pongTimeout = Cancellable.alreadyCancelled,
        nextPing = Cancellable.alreadyCancelled,
        orderBookSubscriptions = Queue.empty,
        addressSubscriptions = Queue.empty,
        maybeRatesUpdateId = None
      )
    }

  private object ClientActorStopped extends RuntimeException("The client actor is stopped") with NoStackTrace
  private val clientActorStoppedEvent = Event.Completed(ClientActorStopped.asLeft)
}
