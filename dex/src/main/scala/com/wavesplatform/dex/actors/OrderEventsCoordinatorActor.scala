package com.wavesplatform.dex.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.{Success, Try}

// TODO tests
object OrderEventsCoordinatorActor {

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Process(event: Events.Event) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyUpdates(updates: Updates) extends Command
  }

  sealed trait Event extends Message

  object Event {
    case class TxChecked(tx: ExchangeTransaction, isForged: Try[Boolean]) extends Event
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    spendableBalancesRef: classic.ActorRef,
    txWriterRef: classic.ActorRef,
    broadcastRef: classic.ActorRef,
    createTransaction: CreateTransaction,
    isTransactionForged: ExchangeTransaction.Id => Future[Boolean]
  ): Behavior[Message] = Behaviors.setup { _ =>
    def default(state: State): Behavior[Message] =
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
                    context.pipeToSelf(isTransactionForged(tx.id()))(Event.TxChecked(tx, _))
                    default(state.addPending(tx.id(), event, tx.buyOrder.id(), tx.sellOrder.id()))

                  case Left(ex) =>
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
                default(state.addPending(event, addressDirectoryRef))
            }

          case Command.ProcessError(event) =>
            addressDirectoryRef ! event
            Behaviors.same

          case Command.ApplyUpdates(updates) =>
            spendableBalancesRef ! SpendableBalancesActor.Command.UpdateStates(updates.updatedBalances) // TODO: AddressActor should receive Balance AND OrderExecuted
            if (updates.failedTxIds.nonEmpty || updates.forgedTxIds.nonEmpty) {
              val failedTxsStr = if (updates.failedTxIds.isEmpty) "" else updates.failedTxIds.mkString("Failed: ", ", ", ". ")
              val forgedTxsStr = if (updates.forgedTxIds.isEmpty) "" else updates.forgedTxIds.mkString("Forged: ", ", ", "")
              context.log.info(s"$failedTxsStr$forgedTxsStr")
            }
            default(state.finishTxs(updates.failedTxIds.union(updates.forgedTxIds), addressDirectoryRef))

          case Event.TxChecked(tx, isForged) =>
            if (state.has(tx.id()))
              isForged match {
                case Success(false) =>
                  broadcastRef ! ExchangeTransactionCreated(tx)
                  Behaviors.same
                case _ => default(state.finishTxs(Set(tx.id()), addressDirectoryRef)) // log error?
              }
            else Behaviors.same // Seems we got it in ApplyUpdates
        }
      }

    default(State(Map.empty, Map.empty))
  }

  private case class State(pendingTxs: Map[ExchangeTransaction.Id, PendingTx], pendingOrders: Map[Order.Id, PendingOrder]) {

    def has(txId: ExchangeTransaction.Id): Boolean = pendingTxs.contains(txId)

    def addPending(txId: ExchangeTransaction.Id, event: Events.OrderExecuted, order1Id: Order.Id, order2Id: Order.Id): State =
      State(
        pendingTxs = pendingTxs.updated(txId, PendingTx(txId, event, Set(order1Id, order2Id))),
        pendingOrders = pendingOrders
          .updated(order1Id, addPendingTx(txId, order1Id))
          .updated(order2Id, addPendingTx(txId, order2Id))
      )

    private def addPendingTx(txId: ExchangeTransaction.Id, order1Id: Order.Id): PendingOrder =
      pendingOrders.get(order1Id) match {
        case Some(x) => x.copy(pendingTxs = x.pendingTxs + txId)
        case None => PendingOrder(order1Id, Set(txId), pendingCancel = none)
      }

    def addPending(event: Events.OrderCanceled, addressDirectoryRef: classic.ActorRef): State = {
      val orderId = event.acceptedOrder.id
      pendingOrders.get(orderId) match {
        case Some(x) => State(pendingTxs, pendingOrders.updated(orderId, x.withCancel(event)))
        case _ =>
          addressDirectoryRef ! event
          this
      }
    }

    def finishTxs(txIds: Set[ExchangeTransaction.Id], addressDirectoryRef: classic.ActorRef): State = {
      val r = txIds.foldLeft((pendingTxs, pendingOrders)) {
        case (r @ (pendingTxs, pendingOrders), txId) =>
          pendingTxs.get(txId) match {
            case None => r
            case Some(pendingTx) =>
              addressDirectoryRef ! pendingTx.event
              val updatedPendingOrders = pendingTx.orderIds.foldLeft(pendingOrders) { case (r, orderId) =>
                applyTx(r, txId, orderId, addressDirectoryRef)
              }
              (pendingTxs - txId, updatedPendingOrders)
          }
      }
      Function.tupled(State)(r)
    }

    private def applyTx(
      pendingOrders: Map[Order.Id, PendingOrder],
      txId: ExchangeTransaction.Id,
      orderId: Order.Id,
      addressDirectoryRef: classic.ActorRef
    ): Map[Order.Id, PendingOrder] =
      pendingOrders.get(orderId) match {
        case Some(pendingOrder) =>
          pendingOrder.applyTx(txId) match {
            case Some(x) => pendingOrders.updated(orderId, x)
            case _ =>
              pendingOrder.pendingCancel.foreach(addressDirectoryRef ! _)
              pendingOrders - orderId
          }

        case _ => pendingOrders
      }

  }

  private case class PendingTx(id: ExchangeTransaction.Id, event: Events.OrderExecuted, orderIds: Set[Order.Id])

  private case class PendingOrder(id: Order.Id, pendingTxs: Set[ExchangeTransaction.Id], pendingCancel: Option[Events.OrderCanceled]) {

    def applyTx(id: ExchangeTransaction.Id): Option[PendingOrder] = {
      val updatedPendingTxs = pendingTxs - id
      if (updatedPendingTxs.isEmpty) none
      else copy(pendingTxs = updatedPendingTxs).some
    }

    def withCancel(event: Events.OrderCanceled): PendingOrder = if (pendingCancel.isEmpty) copy(pendingCancel = event.some) else this
  }

}
