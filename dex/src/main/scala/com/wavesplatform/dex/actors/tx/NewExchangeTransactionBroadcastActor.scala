package com.wavesplatform.dex.actors.tx

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import cats.syntax.option._
import com.wavesplatform.dex.actors.events.OrderEventsCoordinatorActor
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.CheckedBroadcastResult

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

/**
  * Sends transactions to Waves NODE until it is confirmed or a timeout exceeds.
  * Waves NODE doesn't guarantees that a valid transaction will be added to UTX without errors.
  * For example, we can send a valid transaction 2 times in parallel and Waves NODE could return an error
  *   without adding a transaction to UTX.
  */
object NewExchangeTransactionBroadcastActor {

  case class Settings(interval: FiniteDuration, maxPendingTime: FiniteDuration)

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {

    // TODO Message type coupling
    case class Broadcast(clientRef: ActorRef[OrderEventsCoordinatorActor.Message], tx: ExchangeTransaction) extends Command
    case class ProcessConfirmed(txIds: immutable.Iterable[ExchangeTransaction.Id]) extends Command

    // Send once to start
    case object Tick extends Command

  }

  sealed trait Event extends Message

  object Event {

    case class BroadcastedOne(
      clientRef: Option[ActorRef[OrderEventsCoordinatorActor.Message]],
      tx: ExchangeTransaction,
      result: Try[CheckedBroadcastResult]
    ) extends Event

  }

  @FunctionalInterface trait BlockchainInteraction {
    def broadcast(tx: ExchangeTransaction): Future[CheckedBroadcastResult]
  }

  def apply(
    settings: Settings,
    blockchain: BlockchainInteraction
  ): Behavior[Message] = Behaviors.setup { _ =>
    val defaultRestAttempts = math.min((settings.maxPendingTime / settings.interval).intValue, 1) - 1

    def broadcast(
      context: ActorContext[Message],
      tx: ExchangeTransaction,
      clientRef: Option[ActorRef[OrderEventsCoordinatorActor.Message]] = None
    ): Unit = context.pipeToSelf(blockchain.broadcast(tx))(Event.BroadcastedOne(clientRef, tx, _))

    def default(inProgress: Map[ExchangeTransaction.Id, InProgressItem]): Behavior[Message] =
      Behaviors.receive[Message] { (context, message) =>
        message match {
          case message: Command.Broadcast =>
            broadcast(context, message.tx, message.clientRef.some)
            default(inProgress.updated(message.tx.id(), InProgressItem(message.tx, defaultRestAttempts)))

          case message: Command.ProcessConfirmed => default(inProgress -- message.txIds)

          case Command.Tick =>
            val updatedInProgress = inProgress.view.mapValues(_.decreasedAttempts).filter(_._2.isValid).toMap
            if (updatedInProgress.nonEmpty) updatedInProgress.values.view.map(_.tx).foreach(broadcast(context, _))
            context.scheduleOnce(settings.interval, context.self, Command.Tick)
            default(updatedInProgress)

          case message: Event.BroadcastedOne =>
            val txId = message.tx.id()
            if (inProgress.contains(txId)) {
              val (observed, confirmed) = message.result match {
                case Success(x) => (true, x == CheckedBroadcastResult.Confirmed)
                case Failure(e) =>
                  context.log.warn(s"Broadcast failed for $txId", e)
                  (false, false)
              }

              message.clientRef.foreach { clientRef =>
                if (observed) clientRef ! OrderEventsCoordinatorActor.Command.ApplyObserved(message.tx)
              }

              val updatedInProgress = if (confirmed) inProgress - txId else inProgress
              default(updatedInProgress)
            } else Behaviors.same
        }
      }

    default(Map.empty)
  }

  private case class InProgressItem(tx: ExchangeTransaction, restAttempts: Int) {
    def decreasedAttempts: InProgressItem = copy(restAttempts = restAttempts - 1)
    def isValid: Boolean = restAttempts >= 0
  }

}
