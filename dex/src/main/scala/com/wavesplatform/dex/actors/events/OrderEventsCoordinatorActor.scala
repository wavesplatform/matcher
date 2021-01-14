package com.wavesplatform.dex.actors.events

import akka.actor.typed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Confirmed, Command => Broadcaster}
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.{AddressBalanceUpdates, WavesNodeUpdates}
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.grpc.integration.ops.SignedTransactionOps.Implicits
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

// TODO DEX-1042
object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case object Start extends Command

    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyUpdates(updates: WavesNodeUpdates) extends Command

    // Can be sent only from ExchangeTransactionBroadcastActor
    case class ApplyConfirmed(tx: ExchangeTransaction) extends Command
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    dbWriterRef: classic.ActorRef,
    broadcasterRef: typed.ActorRef[Broadcaster],
    createTransaction: CreateTransaction
  ): Behavior[Message] = Behaviors.setup { context =>
    def sendResolved(resolved: Map[Address, PendingAddress]): Unit = resolved.foreach { case (address, x) =>
      addressDirectoryRef ! AddressDirectoryActor.Envelope(
        address,
        AddressActor.Command.ApplyBatch(x.events, x.stashedBalance)
      )
    }

    def sendBalances(updates: AddressBalanceUpdates): Unit = update.view.filter(_._2.nonEmpty).foreach { case (address, balances) =>
      addressDirectoryRef ! AddressDirectoryActor.Envelope(
        address,
        AddressActor.Message.BalanceChanged(balances.keySet, balances)
      )
    }

    def txsToString(label: String, xs: Map[ExchangeTransaction.Id, _]): String = s"$label=${xs.keys.mkString(", ")}"

    val broadcastAdapter: ActorRef[Confirmed] = context.messageAdapter[Confirmed] {
      case Confirmed(tx) => Command.ApplyConfirmed(tx)
    }

    def holdUntilAppearOnNode(state: OrderEventsCoordinatorActorState): Behavior[Message] =
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
                    dbWriterRef ! txCreated
                    broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, tx)

                    val (updatedState, resolved) = state.withKnownOnMatcher(tx.id(), event)
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
              s"Got ApplyUpdates(${txsToString("u", updates.unconfirmedTxs)}, " +
              s"${txsToString("c", updates.confirmedTxs)}, ${txsToString("f", updates.failedTxs)})"
            )

            if (updates.confirmedTxs.nonEmpty) broadcasterRef ! Broadcaster.ProcessConfirmed(updates.confirmedTxs.keySet)

            // All transactions are exchange and from this matcher's account
            val (updatedState1, balancesAfterTxs) = (updates.unconfirmedTxs ++ updates.confirmedTxs ++ updates.failedTxs)
              .filterNot { case (txId, _) => state.knownOnNodeCache.contains(txId) }
              .foldLeft((state, updates.balanceUpdates)) {
                case ((state, restBalances), (txId, tx)) if tx.tx.isExchangeTransaction =>
                  val traderAddresses = tx.tx.exchangeTransactionTraders

                  val (updatedState, updatedRestBalances, resolved) = state.withKnownOnNode(traderAddresses, txId, restBalances)
                  sendResolved(resolved)

                  (updatedState, updatedRestBalances)

                case (r, _) => r
              }

            val (updatedState2, balancesToSend) = updatedState1.withBalanceUpdates(balancesAfterTxs)
            sendBalances(balancesToSend)

            holdUntilAppearOnNode(updatedState2)

          case Command.ApplyConfirmed(tx) =>
            // Why we react only if:
            // * If it was before in UTX, then we received this before during UtxSwitched
            // * If it was added to UTX, then we will receive it
            // * If it won't retry, then we haven't received (neither as unconfirmed, nor confirmed) it and won't receive
            val (updated, restBalances, resolved) = state.withKnownOnNode(tx.traders, tx.id(), Map.empty)
            sendResolved(resolved)
            sendBalances(restBalances) // Actually, they should be empty, but anyway
            holdUntilAppearOnNode(updated)

          case Command.Start =>
            context.log.error("Unexpected Command.Start")
            Behaviors.same
        }
      }

    val pass = Behaviors.receive[Message] { (context, message) =>
      message match {
        case Command.Process(event) =>
          addressDirectoryRef ! event
          event match {
            case event: Events.OrderExecuted =>
              createTransaction(event).foreach { tx =>
                val txCreated = ExchangeTransactionCreated(tx)
                dbWriterRef ! txCreated
                broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, tx)
              }

            case _ =>
          }
          Behaviors.same

        case Command.ProcessError(event) =>
          addressDirectoryRef ! event
          Behaviors.same

        case Command.ApplyUpdates(updates) =>
          sendBalances(updates.balanceUpdates)
          Behaviors.same

        case Command.Start => holdUntilAppearOnNode(OrderEventsCoordinatorActorState(Map.empty, FifoSet.limited(10000))) // TODO DEX-1042 settings
        case _: Command.ApplyConfirmed => Behaviors.same
      }
    }

    pass
  }

}
