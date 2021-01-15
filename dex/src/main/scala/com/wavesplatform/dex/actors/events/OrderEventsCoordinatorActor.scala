package com.wavesplatform.dex.actors.events

import akka.actor.typed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import com.wavesplatform.dex.actors.address.AddressDirectoryActor
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Confirmed, Command => Broadcaster}
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.{AddressBalanceUpdates, WavesNodeUpdates}
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
    val broadcastAdapter: ActorRef[Confirmed] = context.messageAdapter[Confirmed] {
      case Confirmed(tx) => Command.ApplyConfirmed(tx)
    }

    val pass = Behaviors.receive[Message] { (context, message) =>
      message match {
        case Command.Process(event) =>
          event match {
            case event: Events.OrderExecuted =>
              createTransaction(event) match {
                case Right(tx) =>
                  val txCreated = ExchangeTransactionCreated(tx)
                  dbWriterRef ! txCreated
                  broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, tx)
                  addressDirectoryRef ! event // TODO txId

                case Left(ex) =>
                  // We just pass an event without touching a state, because this transaction neither created, nor appeared on Node
                  import event._
                  context.log.warn(
                    s"""Can't create tx: $ex
                       |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                       |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin
                  )
                  addressDirectoryRef ! event
              }

            case _ => addressDirectoryRef ! event
          }
          Behaviors.same

        case Command.ProcessError(event) =>
          addressDirectoryRef ! event
          Behaviors.same

        case Command.ApplyUpdates(updates) =>
          addressDirectoryRef ! AddressDirectoryActor.BatchUpdate(updates.updatesByAddresses)
          Behaviors.same

        case Command.Start =>
          Behaviors.same

        case command: Command.ApplyConfirmed =>
          val updatesByAddresses = command.tx.traders.map(_ -> (AddressBalanceUpdates.empty, Set(command.tx.id()))).toMap
          addressDirectoryRef ! AddressDirectoryActor.BatchUpdate(updatesByAddresses)
          Behaviors.same
      }
    }

    pass
  }

}
