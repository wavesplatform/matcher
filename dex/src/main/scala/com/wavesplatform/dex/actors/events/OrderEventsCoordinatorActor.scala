package com.wavesplatform.dex.actors.events

import akka.actor.typed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Confirmed, Command => Broadcaster}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

import scala.collection.immutable.Queue

// TODO DEX-1042
object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyNodeUpdates(updates: WavesNodeUpdates) extends Command

    // Can be sent only from ExchangeTransactionBroadcastActor
    case class ApplyConfirmedByBroadcaster(tx: ExchangeTransaction) extends Command
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    dbWriterRef: classic.ActorRef,
    broadcasterRef: typed.ActorRef[Broadcaster],
    createTransaction: CreateTransaction
  ): Behavior[Message] = Behaviors.setup { context =>
    val broadcastAdapter: ActorRef[Confirmed] = context.messageAdapter[Confirmed] {
      case Confirmed(tx) => Command.ApplyConfirmedByBroadcaster(tx)
    }

    Behaviors.receive[Message] { (context, message) =>
      message match {
        case Command.Process(event) =>
          event match {
            case event: Events.OrderAdded => addressDirectoryRef ! AddressActor.Command.ApplyOrderBookAdded(event)

            case event: Events.OrderExecuted =>
              val tx = createTransaction(event) match {
                case Right(tx) =>
                  val txCreated = ExchangeTransactionCreated(tx)
                  context.log.info(s"Created ${tx.json()}")
                  dbWriterRef ! txCreated
                  broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, tx)
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

            case event: Events.OrderCanceled => addressDirectoryRef ! AddressActor.Command.ApplyOrderBookCanceled(event)
          }

        case Command.ApplyNodeUpdates(updates) =>
          updates.updatesByAddresses.foreach {
            case (address, (balanceUpdates, observedTxs)) =>
              val markObservedCommand = if (observedTxs.isEmpty) none else AddressActor.Command.MarkTxsObserved(observedTxs).some
              val changeBalanceCommand = if (balanceUpdates.isEmpty) none else AddressActor.Command.ChangeBalances(balanceUpdates).some
              val message = (markObservedCommand, changeBalanceCommand) match {
                case (Some(a), Some(b)) => AddressActor.Command.ApplyBatch(a, b).some
                case (a, b) => a.orElse(b)
              }
              message.foreach(addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(address, _))
          }

        case command: Command.ApplyConfirmedByBroadcaster =>
          val addressActorMessage = AddressActor.Command.MarkTxsObserved(Set(command.tx.id()))
          command.tx.traders.foreach(addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(_, addressActorMessage))

        case Command.ProcessError(event) => addressDirectoryRef ! event
      }

      Behaviors.same
    }
  }

}
