package com.wavesplatform.dex.actors.events

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.{actor => classic}
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.BroadcastExchangeTransactionActor
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.grpc.integration.ops.SignedTransactionOps.Implicits
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

// TODO DEX-1042
object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case object Start extends Command

    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyUpdates(updates: WavesNodeUpdates) extends Command
  }

  sealed trait Event extends Message

  object Event {
    case class TxChecked(tx: ExchangeTransaction, isKnown: Try[Boolean]) extends Event
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    txWriterRef: classic.ActorRef,
    broadcastRef: classic.ActorRef,
    createTransaction: CreateTransaction,
    isTransactionKnown: ExchangeTransaction.Id => Future[Boolean] // TODO Move to broadcastRef
  ): Behavior[Message] = Behaviors.setup { _ =>
    def sendResolved(resolved: Map[Address, PendingAddress]): Unit = resolved.foreach { case (address, x) =>
      addressDirectoryRef ! AddressDirectoryActor.Envelope(
        address,
        AddressActor.Command.ApplyBatch(x.events, x.stashedBalance)
      )
    }

    def sendBalances(balances: AddressAssets): Unit = balances.foreach { case (address, balances) =>
      addressDirectoryRef ! AddressDirectoryActor.Envelope(
        address,
        AddressActor.Message.BalanceChanged(balances.keySet, balances)
      )
    }

    def holdUntilAppearOnNode(state: OrderEventsActorState): Behavior[Message] =
      Behaviors.receive[Message] { (context, message) =>
        message match {
          case Command.Process(event) =>
            event match {
              case event: Events.OrderAdded =>
                addressDirectoryRef ! event
                Behaviors.same

              case event: Events.OrderExecuted =>
                createTransaction(event) match {
                  case Right(tx) =>
                    context.log.info(s"Created transaction: $tx")
                    val txCreated = ExchangeTransactionCreated(tx)
                    txWriterRef ! txCreated
                    context.pipeToSelf(isTransactionKnown(tx.id()))(Event.TxChecked(tx, _))

                    val (updatedState, resolved) = state.withExecuted(tx.id(), event)
                    sendResolved(resolved) // TODO DEX-1042 pipeToSelf is needed if resolved?

                    holdUntilAppearOnNode(updatedState)

                  case Left(ex) =>
                    // We just pass an event without touching a state, because this transaction neither created, nor appeared on Node
                    import event._
                    context.log.warn(
                      s"""Can't create tx: $ex
                         |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                         |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin
                    )
                    addressDirectoryRef ! event
                    Behaviors.same
                }

              case event: Events.OrderCanceled =>
                val (updated, x) = state.withPendingCancel(event)
                x.foreach(addressDirectoryRef ! _)
                holdUntilAppearOnNode(updated)
            }

          case Command.ProcessError(event) =>
            addressDirectoryRef ! event // We don't add this to pending, because this event is only for notification for HTTP API
            Behaviors.same

          case Command.ApplyUpdates(updates) =>
            context.log.info(
              s"Got ApplyUpdates(failedTxs=${updates.failedTxs.keys.mkString(", ")}, appearedTxs=${updates.appearedTxs.keys.mkString(", ")})"
            )

            // All transactions are exchange and from this matcher's account
            val (updatedState1, balancesAfterTxs) = (updates.appearedTxs ++ updates.failedTxs)
              .filterNot { case (txId, _) => state.knownOnNodeCache.contains(txId) }
              .foldLeft((state, updates.updatedBalances)) {
                case ((state, restBalances), (txId, tx)) if tx.tx.isExchangeTransaction =>
                  val traderAddresses = tx.tx.exchangeTransactionTraders

                  val (updatedState, updatedRestBalances, resolved) = state.withKnownOnNodeTx(traderAddresses, txId, restBalances)
                  sendResolved(resolved)

                  (updatedState, updatedRestBalances)

                case (r, _) => r
              }

            val (updatedState2, balancesToSend) = updatedState1.withBalanceUpdates(balancesAfterTxs)
            sendBalances(balancesToSend)

            holdUntilAppearOnNode(updatedState2)

          case Event.TxChecked(tx, isKnown) =>
            val txId = tx.id()
            val inCache = state.knownOnNodeCache.contains(txId)
            context.log.info(s"Got TxChecked(${tx.id()}, isKnown=$isKnown), inCache: $inCache")
            if (inCache) Behaviors.same
            else isKnown match {
              case Success(false) =>
                broadcastRef ! BroadcastExchangeTransactionActor.Broadcast(context.self, tx)
                Behaviors.same
              case Failure(e) =>
                context.log.info(s"Failed to check ${tx.id()} status", e)
                val (updated, restBalances, resolved) = state.withKnownOnNodeTx(tx.traders, txId, Map.empty)
                sendResolved(resolved)
                sendBalances(restBalances) // Actually, they should be empty, but anyway
                holdUntilAppearOnNode(updated)
            }

          case Command.Start =>
            context.log.error("Unexpected Command.Start")
            Behaviors.same
        }
      }

    val pass = Behaviors.receiveMessage[Message] {
      case Command.Process(event) =>
        addressDirectoryRef ! event
        event match {
          case event: Events.OrderExecuted =>
            createTransaction(event).foreach { tx =>
              val txCreated = ExchangeTransactionCreated(tx)
              txWriterRef ! txCreated
              broadcastRef ! txCreated
            }

          case _ =>
        }
        Behaviors.same

      case Command.ProcessError(event) =>
        addressDirectoryRef ! event
        Behaviors.same

      case Command.ApplyUpdates(updates) =>
        updates.updatedBalances.foreach { case (address, updates) =>
          addressDirectoryRef ! AddressDirectoryActor.Envelope(address, AddressActor.Message.BalanceChanged(updates.keySet, updates))
        }
        Behaviors.same

      case Command.Start => holdUntilAppearOnNode(OrderEventsActorState(Map.empty, FifoSet.limited(10000))) // TODO DEX-1042 settings
      case _: Event.TxChecked => Behaviors.same
    }

    pass
  }

}
