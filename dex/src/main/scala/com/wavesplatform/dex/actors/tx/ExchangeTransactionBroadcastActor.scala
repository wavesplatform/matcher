package com.wavesplatform.dex.actors.tx

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import cats.syntax.option._
import com.wavesplatform.dex.collections.PositiveMap
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.CheckedBroadcastResult
import com.wavesplatform.dex.time.Time

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * Sends transactions to Waves NODE until:
 * - It is confirmed;
 * - A timeout exceeds;
 * - Errors are insignificant (can_retry = true).
 * Waves NODE doesn't guarantees that a valid transaction will be added to UTX without errors.
 * For example, we can send a valid transaction 2 times in parallel and Waves NODE could return an error
 *   without adding a transaction to UTX.
 * Or the UTX pool could be full, so we will receive "Transaction pool bytes size limit is reached" (or something similar)
 */
object ExchangeTransactionBroadcastActor {

  val ExchangeTransactionExpiration = 120.minutes
  val ExchangeTransactionExpirationMillis = ExchangeTransactionExpiration.toMillis

  case class Settings(interval: FiniteDuration, maxPendingTime: FiniteDuration)

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {

    /**
     * Try to send and reply to clientRef when it fails
     */
    case class Broadcast(clientRef: ActorRef[Observed], addressSpendings: Map[Address, PositiveMap[Asset, Long]], tx: ExchangeTransaction)
        extends Command

    case object Tick extends Command

  }

  sealed trait Event extends Message

  object Event {

    case class Broadcasted(
      clientRef: Option[ActorRef[Observed]],
      addressSpendings: Map[Address, PositiveMap[Asset, Long]],
      tx: ExchangeTransaction,
      result: Try[CheckedBroadcastResult]
    ) extends Event

  }

  @FunctionalInterface trait BlockchainInteraction {
    def broadcast(tx: ExchangeTransaction): Future[CheckedBroadcastResult]
  }

  def apply(
    settings: Settings,
    blockchain: BlockchainInteraction,
    time: Time
  ): Behavior[Message] = Behaviors.withTimers { timer =>
    val timerKey = Command.Tick

    val defaultRestAttempts = math.max((settings.maxPendingTime / settings.interval).intValue, 1) - 1

    def isExpired(tx: ExchangeTransaction): Boolean = (time.correctedTime() - tx.timestamp) > ExchangeTransactionExpirationMillis

    def broadcast(
      context: ActorContext[Message],
      tx: ExchangeTransaction,
      clientRef: Option[ActorRef[Observed]],
      addressSpendings: Map[Address, PositiveMap[Asset, Long]]
    ): Unit = {
      context.log.info(s"Broadcasting ${tx.id()}")
      context.pipeToSelf(blockchain.broadcast(tx))(Event.Broadcasted(clientRef, addressSpendings, tx, _))
    }

    def default(inProgress: Map[ExchangeTransaction.Id, InProgressItem]): Behavior[Message] =
      Behaviors.receive[Message] { (context, message) =>
        message match {
          case message: Command.Broadcast =>
            // It would be better to just send the tx, but we can overload the node
            if (isExpired(message.tx)) {
              message.clientRef ! Observed(message.tx, message.addressSpendings)
              Behaviors.same
            } else {
              broadcast(context, message.tx, message.clientRef.some, message.addressSpendings)
              default(inProgress.updated(message.tx.id(), InProgressItem(message.tx, defaultRestAttempts)))
            }

          case Command.Tick =>
            val updatedInProgress = inProgress.view.mapValues(_.decreasedAttempts)
              .filter { case (_, x) => x.isValid && !isExpired(x.tx) }
              .toMap
            if (updatedInProgress.nonEmpty) updatedInProgress.foreach {
              case (_, x) => broadcast(context, x.tx, none, Map.empty)
            }
            default(updatedInProgress)

          case message: Event.Broadcasted =>
            val txId = message.tx.id()
            val isInProgress = inProgress.contains(txId)
            message.result match {
              case Failure(e) => context.log.warn(s"Failed to broadcast $txId (inProgress=$isInProgress)", e)
              case Success(x) =>
                x match {
                  case CheckedBroadcastResult.Unconfirmed(isNew) =>
                    context.log.info(s"$txId (inProgress=$isInProgress) is unconfirmed${if (isNew) " and is new" else ""}")
                  case CheckedBroadcastResult.Confirmed =>
                    context.log.info(s"$txId (inProgress=$isInProgress) is confirmed")
                  case CheckedBroadcastResult.Failed(message, canRetry) =>
                    context.log.warn(s"Failed to broadcast $txId (inProgress=$isInProgress, canRetry=$canRetry): $message")
                }
            }

            if (isInProgress) {
              val canRetry = message.result match {
                case Success(CheckedBroadcastResult.Confirmed) => false
                case Success(CheckedBroadcastResult.Failed(_, x)) => x
                case _ => true
              }

              if (canRetry) {
                if (!timer.isTimerActive(timerKey)) timer.startSingleTimer(timerKey, Command.Tick, settings.interval)
              } else { // This means, the transaction failed
                // TODO separate from sending

                /**
                 * Unconfirmed (new) - don't reply, because an event come from UTX
                 * Unconfirmed (old) - should reply, because AA could be initialized after tx went to UTX
                 * Confirmed - should reply
                 * Failed (no retry) - should reply
                 * Failed (retry) - don't reply, wait
                 */

                message.clientRef.foreach(_ ! Observed(message.tx, message.addressSpendings))
              }

              val updatedInProgress = if (canRetry) inProgress else inProgress - txId
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

  case class Observed(tx: ExchangeTransaction, addressSpending: Map[Address, PositiveMap[Asset, Long]])

}
