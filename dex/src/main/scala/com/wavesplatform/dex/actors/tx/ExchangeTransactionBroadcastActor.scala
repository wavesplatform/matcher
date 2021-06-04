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
import scala.util.chaining._

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
    case class Broadcasted(tx: ExchangeTransaction, result: Try[CheckedBroadcastResult]) extends Event
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

    val defaultAttempts = math.max((settings.maxPendingTime / settings.interval).intValue, 1) - 1

    def isExpired(tx: ExchangeTransaction): Boolean =
      (time.correctedTime() - tx.timestamp) > ExchangeTransactionExpirationMillis

    def broadcast(context: ActorContext[Message], tx: ExchangeTransaction): Unit = {
      context.log.info(s"Broadcasting ${tx.id()}")
      context.pipeToSelf(blockchain.broadcast(tx))(Event.Broadcasted(tx, _))
    }

    def reply(item: InProgressItem): Unit = item.clientRef.foreach(_ ! Observed(item.tx, item.addressSpendings))

    def default(inProgress: Map[ExchangeTransaction.Id, InProgressItem]): Behavior[Message] =
      Behaviors.receive[Message] { (context, message) =>
        message match {
          case message: Command.Broadcast =>
            // It would be better to just send the tx, but we can overload the node
            if (isExpired(message.tx)) {
              message.clientRef ! Observed(message.tx, message.addressSpendings)
              Behaviors.same
            } else {
              broadcast(context, message.tx)
              default(inProgress.updated(
                message.tx.id(),
                InProgressItem(message.tx, defaultAttempts, message.clientRef.some, message.addressSpendings)
              ))
            }

          case Command.Tick =>
            val updatedInProgress = inProgress.view.mapValues(_.decreasedAttempts)
              .filter {
                case (txId, x) =>
                  val valid = x.isValid // This could be in Event.Broadcasted
                  val expired = isExpired(x.tx)
                  (valid && !expired).tap { retry =>
                    if (retry) broadcast(context, x.tx)
                    else {
                      context.log.warn(s"Failed to broadcast $txId (valid=$valid, expired=$expired)")
                      reply(x)
                    }
                  }
              }
              .toMap
            default(updatedInProgress)

          case message: Event.Broadcasted =>
            val txId = message.tx.id()
            val item = inProgress.get(txId)
            val isInProgress = item.nonEmpty

            message.result match {
              case Failure(e) => context.log.warn(s"Failed to broadcast $txId (inProgress=$isInProgress)", e)
              case Success(x) =>
                x match {
                  case CheckedBroadcastResult.Unconfirmed(isNew) =>
                    context.log.info(s"$txId (inProgress=$isInProgress) is unconfirmed${if (isNew) " and is new" else ""}")
                  case CheckedBroadcastResult.Confirmed =>
                    context.log.info(s"$txId (inProgress=$isInProgress) is confirmed")
                  case CheckedBroadcastResult.Failed(message, canRetry) =>
                    val logMessage = s"Failed to broadcast $txId (inProgress=$isInProgress, canRetry=$canRetry): $message"
                    if (canRetry)
                      context.log.debug(logMessage)
                    else
                      context.log.warn(logMessage)
                }
            }

            item match {
              case None => Behaviors.same
              case Some(item) =>
                val canRetry = message.result match {
                  case Success(CheckedBroadcastResult.Confirmed) => false
                  case Success(CheckedBroadcastResult.Failed(_, canRetry)) => canRetry
                  case _ => true
                }

                val updatedInProgress1 =
                  if (canRetry) {
                    if (!timer.isTimerActive(timerKey)) timer.startSingleTimer(timerKey, Command.Tick, settings.interval)
                    inProgress
                  } else inProgress - txId

                // About AA - it breaks some rules, but this is measured
                val sendReply = message.result match {
                  case Success(CheckedBroadcastResult.Confirmed) =>
                    // We don't know, how old is this tx. AA could not be created
                    true
                  case Success(CheckedBroadcastResult.Failed(_, x)) =>
                    // no retry - should reply, it won't appear in UTX
                    // retry - don't reply, because it hasn't yet appeared in UTX
                    !x
                  case _ =>
                    // Don't reply:
                    // Unconfirmed(new) - because an event will come from UTX and AA is created
                    // Unconfirmed(old) - because an event came from UTX and AA is created
                    // Failed - tx hasn't yet appeared in UTX
                    // AA is created, because we create it on MarkTxsObserved
                    false
                }

                val updatedInProgress2 =
                  if (sendReply) {
                    reply(item)
                    if (canRetry) updatedInProgress1.updatedWith(txId) {
                      case Some(x) => x.copy(clientRef = none).some // Shouldn't send in future
                      case x => x
                    }
                    else updatedInProgress1 // Already removed
                  } else updatedInProgress1

                default(updatedInProgress2)
            }
        }
      }

    default(Map.empty)
  }

  private case class InProgressItem(
    tx: ExchangeTransaction,
    restAttempts: Int,
    clientRef: Option[ActorRef[Observed]],
    addressSpendings: Map[Address, PositiveMap[Asset, Long]]
  ) {
    def decreasedAttempts: InProgressItem = copy(restAttempts = restAttempts - 1)
    def isValid: Boolean = restAttempts >= 0
  }

  case class Observed(tx: ExchangeTransaction, addressSpending: Map[Address, PositiveMap[Asset, Long]])

}
