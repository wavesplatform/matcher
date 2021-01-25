package com.wavesplatform.dex.actors.events

import akka.actor.typed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Observed, Command => Broadcaster}
import com.wavesplatform.dex.collections.FifoSet
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

// TODO DEX-1042
object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyNodeUpdates(updates: WavesNodeUpdates) extends Command

    // Can be sent only from ExchangeTransactionBroadcastActor
    case class ApplyObservedByBroadcaster(tx: ExchangeTransaction) extends Command
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    dbWriterRef: classic.ActorRef,
    broadcasterRef: typed.ActorRef[Broadcaster],
    createTransaction: CreateTransaction
  ): Behavior[Message] = Behaviors.setup { context =>
    val broadcastAdapter: ActorRef[Observed] = context.messageAdapter[Observed] {
      case Observed(tx) => Command.ApplyObservedByBroadcaster(tx)
    }

    def default(knownTxIds: FifoSet[ExchangeTransaction.Id]): Behaviors.Receive[Message] = Behaviors.receive[Message] { (context, message) =>
      message match {
        case Command.Process(event) =>
          event match {
            case event: Events.OrderAdded =>
              addressDirectoryRef ! AddressActor.Command.ApplyOrderBookAdded(event)
              Behaviors.same

            case event: Events.OrderExecuted =>
              val tx = createTransaction(event) match {
                case Right(tx) =>
                  val txId = tx.id()
                  val txCreated = ExchangeTransactionCreated(tx)
                  context.log.info(s"Created ${tx.json()}")
                  dbWriterRef ! txCreated

                  if (knownTxIds.contains(txId)) {
                    val addressActorMessage = AddressActor.Command.MarkTxsObserved(Set(txId))
                    tx.traders.foreach(addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(_, addressActorMessage))
                  } else broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, tx)

                  tx.some

                case Left(e) =>
                  // We don't touch a state, because this transaction neither created, nor appeared on Node
                  import event._
                  context.log.warn(
                    s"""Can't create tx: $e
                       |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                       |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin
                  )
                  none
              }
              addressDirectoryRef ! AddressActor.Command.ApplyOrderBookExecuted(event, tx)
              Behaviors.same

            case event: Events.OrderCanceled =>
              addressDirectoryRef ! AddressActor.Command.ApplyOrderBookCanceled(event)
              Behaviors.same
          }

        case Command.ApplyNodeUpdates(updates) =>
          val (updatedKnownTxIds, filteredObservedTxs) = updates.observedTxs.keys.foldLeft((knownTxIds, List.empty[ExchangeTransaction.Id])) {
            case ((knownTxIds, exclude), txId) =>
              val (updatedKnownTxIds, isNew) = knownTxIds.append(txId)
              val updatedExclude = if (isNew) exclude else txId :: exclude
              (updatedKnownTxIds, updatedExclude)
          }

          updates.copy(observedTxs = updates.observedTxs -- filteredObservedTxs).updatesByAddresses.foreach {
            case (address, (balanceUpdates, observedTxs)) =>
              val markObservedCommand = if (observedTxs.isEmpty) none else AddressActor.Command.MarkTxsObserved(observedTxs).some
              val changeBalanceCommand = if (balanceUpdates.isEmpty) none else AddressActor.Command.ChangeBalances(balanceUpdates).some
              val message = (markObservedCommand, changeBalanceCommand) match {
                case (Some(a), Some(b)) => AddressActor.Command.ApplyBatch(a, b).some
                case (a, b) => a.orElse(b)
              }
              message.foreach(addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(address, _))
          }

          default(updatedKnownTxIds)

        case command: Command.ApplyObservedByBroadcaster =>
          val txId = command.tx.id()
          val addressActorMessage = AddressActor.Command.MarkTxsObserved(Set(txId))
          command.tx.traders.foreach(addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(_, addressActorMessage))
          val (updatedKnownTxIds, _) = knownTxIds.append(txId)
          default(updatedKnownTxIds)

        case Command.ProcessError(event) =>
          addressDirectoryRef ! event
          Behaviors.same
      }
    }

    default(FifoSet.limited[ExchangeTransaction.Id](10000)) // TODO Why 10000?
  }

}
