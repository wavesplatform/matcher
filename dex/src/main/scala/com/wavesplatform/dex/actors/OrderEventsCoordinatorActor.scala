package com.wavesplatform.dex.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.{actor => classic}
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyUpdates(updates: Updates) extends Command
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    spendableBalancesRef: classic.ActorRef,
    txWriterRef: classic.ActorRef,
    broadcastRef: classic.ActorRef,
    createTransaction: CreateTransaction
  ): Behavior[Message] = Behaviors.setup { context =>
    Behaviors.receiveMessage[Message] {
      case Command.Process(event) =>
        event match {
          case event: Events.OrderAdded =>
            addressDirectoryRef ! event

          case event: Events.OrderExecuted =>
            createTransaction(event) match {
              case Right(tx) =>
                context.log.info(s"Created transaction: $tx")
                val txCreated = ExchangeTransactionCreated(tx)
                txWriterRef ! txCreated
                broadcastRef ! txCreated
                addressDirectoryRef ! event

              case Left(ex) =>
                import event._
                context.log.warn(
                  s"""Can't create tx: $ex
                     |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                     |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin
                )
            }
            addressDirectoryRef ! event

          case event: Events.OrderCanceled =>
            addressDirectoryRef ! event
        }
        Behaviors.same

      case Command.ProcessError(event) =>
        addressDirectoryRef ! event
        Behaviors.same

      case Command.ApplyUpdates(updates) =>
        spendableBalancesRef ! SpendableBalancesActor.Command.UpdateStates(updates.updatedBalances)
        Behaviors.same
    }
  }

}
