package com.wavesplatform.dex.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.{actor => classic}
import cats.syntax.option._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.fp.MapImplicits.MapOps
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

import scala.collection.immutable.Queue
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
    case class TxChecked(tx: ExchangeTransaction, isKnown: Try[Boolean]) extends Event
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    spendableBalancesRef: classic.ActorRef,
    txWriterRef: classic.ActorRef,
    broadcastRef: classic.ActorRef,
    createTransaction: CreateTransaction,
    isTransactionKnown: ExchangeTransaction.Id => Future[Boolean]
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
                    context.pipeToSelf(isTransactionKnown(tx.id()))(Event.TxChecked(tx, _))
                    default(state.withExecuted(tx.id(), event, addressDirectoryRef))

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
                default(state.withPendingCancel(event, addressDirectoryRef))
            }

          case Command.ProcessError(event) =>
            addressDirectoryRef ! event // We don't add this to pending, because this event is only for notification for HTTP API
            Behaviors.same

          case Command.ApplyUpdates(updates) =>
            // addressDirectory ! AddressDirectoryActor.Envelope(address, AddressActor.Message.BalanceChanged(stateUpdate.keySet, stateUpdate))
            val txAddresses = (updates.appearedTxs.values ++ updates.failedTxs.values).map()
            updates.updatedBalances.view.filterKeys()
            val (updatedState, restChanges) = state.withBalanceUpdates(updates.updatedBalances)

            if (restChanges.nonEmpty) spendableBalancesRef ! SpendableBalancesActor.Command.UpdateStates(restChanges)
            // updates.failedTxs // TODO
            default(updatedState.withKnownOnNodeTxs(updates.appearedTxs.keySet, addressDirectoryRef))

          case Event.TxChecked(tx, isKnown) =>
            val txId = tx.id()
            isKnown match {
              case Success(false) =>
                broadcastRef ! ExchangeTransactionCreated(tx) // TODO handle invalid
                Behaviors.same
              case _ =>
                // log error?
                default(
                  List(
                    tx.buyOrder.senderPublicKey.toAddress,
                    tx.sellOrder.senderPublicKey.toAddress
                  ).foldLeft(state) { case (r, address) => r.withKnownOnNodeTx(address, txId, Map.empty, addressDirectoryRef) }
                )
            }
        }
      }

    default(State(Map.empty))
  }

  private case class State(addresses: Map[Address, PendingAddress]) {

    def withBalanceUpdates(updates: AddressAssets): (State, AddressAssets) =
      updates.foldLeft((this, Map.empty: AddressAssets)) {
        case ((state, rest), x @ (address, updates)) =>
          state.addresses.get(address) match {
            case None => (state, rest + x)
            case Some(x) =>
              (
                state.copy(state.addresses.updated(address, x.withUpdatedBalances(updates))),
                rest
              )
          }
      }

    def withExecuted(txId: ExchangeTransaction.Id, event: Events.OrderExecuted, addressDirectoryRef: classic.ActorRef): State = {
      lazy val defaultPendingAddress = PendingAddress(
        pendingTxs = Map[ExchangeTransaction.Id, PendingTransactionType](txId -> PendingTransactionType.KnownOnMatcher),
        stashedBalance = Map.empty,
        events = Queue(event)
      )

      State(
        List(
          event.counter.order.senderPublicKey.toAddress,
          event.submitted.order.senderPublicKey.toAddress
        ).foldLeft(addresses) { case (addresses, address) =>
          val pendingAddress = addresses.get(address).fold(defaultPendingAddress)(_.withKnownOnMatcher(txId, event))
          if (pendingAddress.isResolved) {
            addressDirectoryRef ! AddressDirectoryActor.Envelope(
              address,
              AddressActor.Command.ApplyBatch(pendingAddress.events, pendingAddress.stashedBalance)
            )
            addresses - address
          } else addresses.updated(address, pendingAddress)
        }
      )
    }

    def withPendingCancel(event: Events.OrderCanceled, addressDirectoryRef: classic.ActorRef): State = {
      val address = event.acceptedOrder.order.senderPublicKey.toAddress
      addresses.get(address) match {
        case Some(x) => State(addresses.updated(address, x.withEvent(event)))
        case _ =>
          addressDirectoryRef ! event
          this
      }
    }

    def withKnownOnNodeTx(
      trader: Address,
      txId: ExchangeTransaction.Id,
      balanceUpdates: Map[Asset, Long],
      addressDirectoryRef: classic.ActorRef
    ): State =
      State(addresses.get(trader) match {
        case Some(pendingAddress) =>
          val updatedPendingAddress = pendingAddress.withKnownOnNode(txId, balanceUpdates)
          if (updatedPendingAddress.isResolved) {
            addressDirectoryRef ! AddressDirectoryActor.Envelope(
              trader,
              AddressActor.Command.ApplyBatch(updatedPendingAddress.events, updatedPendingAddress.stashedBalance)
            )
            addresses - trader
          } else addresses.updated(trader, updatedPendingAddress)

        case None =>
          addressDirectoryRef ! AddressDirectoryActor.Envelope(
            trader,
            AddressActor.Message.BalanceChanged(balanceUpdates.keySet, balanceUpdates) // TODO
          )
          addresses
      })

  }

  private case class PendingAddress(
    pendingTxs: Map[ExchangeTransaction.Id, PendingTransactionType],
    stashedBalance: Map[Asset, Long],
    events: Queue[Events.Event]
  ) {
    def isResolved: Boolean = pendingTxs.isEmpty

    def withUpdatedBalances(xs: Map[Asset, Long]): PendingAddress = copy(stashedBalance = stashedBalance ++ xs)

    def withKnownOnNode(txId: ExchangeTransaction.Id, balanceUpdates: Map[Asset, Long]): PendingAddress =
      pendingTxs.get(txId) match {
        case Some(PendingTransactionType.KnownOnNode) => this
        case Some(PendingTransactionType.KnownOnMatcher) =>
          copy(
            pendingTxs = pendingTxs - txId,
            stashedBalance = stashedBalance ++ balanceUpdates
          )
        case _ =>
          copy(
            pendingTxs = pendingTxs.updated(txId, PendingTransactionType.KnownOnNode),
            stashedBalance = stashedBalance ++ balanceUpdates
          )
      }

    def withKnownOnMatcher(txId: ExchangeTransaction.Id, event: Events.OrderExecuted): PendingAddress =
      pendingTxs.get(txId) match {
        case Some(PendingTransactionType.KnownOnMatcher) => this
        case Some(PendingTransactionType.KnownOnNode) =>
          copy(
            pendingTxs = pendingTxs - txId,
            events = events.enqueue(event)
          )
        case _ =>
          copy(
            pendingTxs = pendingTxs.updated(txId, PendingTransactionType.KnownOnMatcher),
            events = events.enqueue(event)
          )
      }

    def withEvent(event: Events.Event): PendingAddress = copy(events = events.enqueue(event))
  }

  sealed private trait PendingTransactionType

  private object PendingTransactionType {
    case object KnownOnMatcher extends PendingTransactionType

    // Will be known soon on Matcher. Another case is impossible, because we check a transaction first
    case object KnownOnNode extends PendingTransactionType
  }

}
