package com.wavesplatform.dex.actors

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.{actor => classic}
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.WavesBlockchainClient.Updates
import com.wavesplatform.dex.grpc.integration.clients.domain.portfolio.AddressAssets
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.util.{Success, Try}

// TODO tests
object OrderEventsCoordinatorActor extends ScorexLogging {

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
            context.log.info(s"Got ApplyUpdates($updates)")
            // ISSUE HERE! A tx appeared in UTX then in FORGED! <------
            val (updatedState, restBalances) = (updates.appearedTxs ++ updates.failedTxs).foldLeft((state, updates.updatedBalances)) {
              case (r @ (state, restBalances), (txId, tx)) =>
                tx.tx.transaction match {
                  case Some(body) if body.data.isExchange =>
                    val traderAddresses = tx.tx.transaction.flatMap(_.data.exchange).to(Seq).flatMap { data =>
                      data.orders.map(_.senderPublicKey.toVanillaPublicKey.toAddress)
                    }

                    val updatedState = traderAddresses.foldLeft(state) { case (state, address) =>
                      state.withKnownOnNodeTx(address, txId, restBalances.getOrElse(address, Map.empty), addressDirectoryRef)
                    }

                    (updatedState, restBalances -- traderAddresses)

                  case _ => r
                }
            }

            default(updatedState.withBalanceUpdates(restBalances, addressDirectoryRef))

          case Event.TxChecked(tx, isKnown) =>
            val txId = tx.id()
            context.log.info(s"Got TxChecked($tx, $isKnown)")
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

    def withBalanceUpdates(updates: AddressAssets, addressDirectoryRef: classic.ActorRef): State =
      State(
        updates.foldLeft(addresses) {
          case (addresses, (address, updates)) =>
            addresses.get(address) match {
              case Some(x) => addresses.updated(address, x.withUpdatedBalances(updates))
              case None =>
                // TODO
                addressDirectoryRef ! AddressDirectoryActor.Envelope(
                  address,
                  AddressActor.Message.BalanceChanged(updates.keySet, updates)
                )
                addresses
            }
        }
      )

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
          log.info(s"==> State.withExecuted: $txId, $address, isResolved: ${pendingAddress.isResolved}, $pendingAddress")
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
      address: Address,
      txId: ExchangeTransaction.Id,
      balanceUpdates: Map[Asset, Long],
      addressDirectoryRef: classic.ActorRef
    ): State =
      State(addresses.get(address) match {
        case Some(pendingAddress) =>
          val updatedPendingAddress = pendingAddress.withKnownOnNode(txId, balanceUpdates)
          log.info(s"==> State.withKnownOnNodeTx: $txId, $address, isResolved: ${updatedPendingAddress.isResolved}, $updatedPendingAddress")
          if (updatedPendingAddress.isResolved) {
            addressDirectoryRef ! AddressDirectoryActor.Envelope(
              address,
              AddressActor.Command.ApplyBatch(updatedPendingAddress.events, updatedPendingAddress.stashedBalance)
            )
            addresses - address
          } else addresses.updated(address, updatedPendingAddress)

        case None =>
          log.info(s"==> State.withKnownOnNodeTx: $txId, $address, send only balances")
          addressDirectoryRef ! AddressDirectoryActor.Envelope(
            address,
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
