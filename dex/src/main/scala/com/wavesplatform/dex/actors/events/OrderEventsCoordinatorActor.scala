package com.wavesplatform.dex.actors.events

import akka.actor.typed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.data.NonEmptyList
import cats.instances.long._
import cats.instances.map._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.actors.address.AddressActor.Command.ObservedTxData
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Observed, Command => Broadcaster}
import com.wavesplatform.dex.collections.{FifoSet, PositiveMap}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.transaction.ExchangeTransaction
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeUpdates
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.dex.history.HistoryRouterActor.HistoryInsertMsg
import com.wavesplatform.dex.model.Events
import com.wavesplatform.dex.model.Events.ExchangeTransactionCreated
import com.wavesplatform.dex.model.ExchangeTransactionCreator.CreateTransaction
import play.api.libs.json.Json

object OrderEventsCoordinatorActor {

  final private case class AddressActorCommands(
    added: Vector[AddressActor.Command.ApplyOrderBookAdded] = Vector.empty,
    executed: Vector[AddressActor.OrderBookExecutedEvent] = Vector.empty,
    cancelled: Vector[AddressActor.Command.ApplyOrderBookCanceled] = Vector.empty
  ) {
    def add(event: AddressActor.Command.ApplyOrderBookAdded): AddressActorCommands = copy(added = added :+ event)
    def add(event: AddressActor.OrderBookExecutedEvent): AddressActorCommands = copy(executed = executed :+ event)
    def add(event: AddressActor.Command.ApplyOrderBookCanceled): AddressActorCommands = copy(cancelled = cancelled :+ event)
  }

  case class Settings(exchangeTransactionCacheSize: Int)

  sealed trait Message extends Product with Serializable

  sealed trait Command extends Message

  object Command {
    case class Process(events: NonEmptyList[Events.Event]) extends Command
    case class ProcessError(event: Events.OrderCancelFailed) extends Command
    case class ApplyNodeUpdates(updates: WavesNodeUpdates) extends Command

    // Can be sent only from ExchangeTransactionBroadcastActor
    case class ApplyObservedByBroadcaster(tx: ExchangeTransaction, addressSpending: Map[Address, PositiveMap[Asset, Long]]) extends Command
  }

  def apply(
    settings: Settings,
    addressDirectoryRef: classic.ActorRef,
    dbWriterRef: classic.ActorRef,
    broadcasterRef: typed.ActorRef[Broadcaster],
    createTransaction: CreateTransaction,
    historyRouterRef: Option[classic.ActorRef]
  ): Behavior[Message] = apply(
    addressDirectoryRef,
    dbWriterRef,
    broadcasterRef,
    createTransaction,
    FifoSet.limited[ExchangeTransaction.Id](settings.exchangeTransactionCacheSize),
    historyRouterRef: Option[classic.ActorRef]
  )

  def apply(
    addressDirectoryRef: classic.ActorRef,
    dbWriterRef: classic.ActorRef,
    broadcasterRef: typed.ActorRef[Broadcaster],
    createTransaction: CreateTransaction,
    observedTxIds: FifoSet[ExchangeTransaction.Id],
    historyRouterRef: Option[classic.ActorRef]
  ): Behavior[Message] = Behaviors.setup { context =>
    val broadcastAdapter: ActorRef[Observed] = context.messageAdapter[Observed] {
      case Observed(tx, addressSpending) => Command.ApplyObservedByBroadcaster(tx, addressSpending)
    }

    def sendEventToHistoryRouter(event: Events.Event): Unit = historyRouterRef.foreach { historyRouterRef =>
      val msg = event match {
        case Events.OrderAdded(lo, _, timestamp) => HistoryInsertMsg.SaveOrder(lo, timestamp)
        case e: Events.OrderExecuted => HistoryInsertMsg.SaveEvent(e)
        case e: Events.OrderCanceled => HistoryInsertMsg.SaveEvent(e)
      }
      historyRouterRef ! msg
    }

    def default(): Behaviors.Receive[Message] = Behaviors.receive[Message] { (context, message) =>
      message match {
        // DEX-1192 docs/places-and-cancels.md
        case Command.Process(events) =>
          val addressActorCommands =
            events.foldLeft(AddressActorCommands()) { case (acc, event) =>
              event match {
                case event: Events.OrderAdded =>
                  acc.add(AddressActor.Command.ApplyOrderBookAdded(event))

                case event: Events.OrderExecuted =>
                  // If we here, AddressActor is guaranteed to be created, because this happens only after Events.OrderAdded
                  val createTxResult = createTransaction(event)
                  createTxResult.toEither match {
                    case Right(tx) =>
                      val txCreated = ExchangeTransactionCreated(createTxResult.transaction)
                      context.log.info(s"Created ${createTxResult.transaction.json()}")
                      dbWriterRef ! txCreated

                      val addressSpendings =
                        Map(event.counter.order.sender.toAddress -> PositiveMap(event.counterExecutedSpending)) |+|
                        Map(event.submitted.order.sender.toAddress -> PositiveMap(event.submittedExecutedSpending))

                      broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, addressSpendings, tx)

                    case Left(e) =>
                      // We don't touch a state, because this transaction neither created, nor appeared on Node
                      import event._
                      context.log.warn(
                        s"""Can't create tx: $e
                           |o1: (amount=${submitted.amount}, fee=${submitted.fee}): ${Json.prettyPrint(submitted.order.json())}
                           |o2: (amount=${counter.amount}, fee=${counter.fee}): ${Json.prettyPrint(counter.order.json())}""".stripMargin
                      )
                  }
                  // We don't update "observedTxIds" here, because expectedTx relates to "createdTxs"
                  acc.add(AddressActor.OrderBookExecutedEvent(event, createTxResult))

                case event: Events.OrderCanceled =>
                  // If we here, AddressActor is guaranteed to be created, because this happens only after Events.OrderAdded
                  acc.add(AddressActor.Command.ApplyOrderBookCanceled(event))

              }
            }

          addressActorCommands.added.foreach { event =>
            sendEventToHistoryRouter(event.event)
            addressDirectoryRef ! event
          }
          addressActorCommands.executed.foldLeft(Map.empty[Address, Vector[AddressActor.OrderBookExecutedEvent]]) { case (acc, event) =>
            sendEventToHistoryRouter(event.event)
            event.affectedAddresses.foldLeft(acc) { case (acc, adr) =>
              acc.updated(adr, acc.getOrElse(adr, Vector.empty) :+ event)
            }
          }.foreach { case (adr, events) =>
            NonEmptyList
              .fromList(events.toList)
              .foreach(addressDirectoryRef ! AddressActor.Command.ApplyOrderBookExecuted(adr, _))
          }
          addressActorCommands.cancelled.foreach { event =>
            sendEventToHistoryRouter(event.event)
            addressDirectoryRef ! event
          }

          Behaviors.same

        case Command.ApplyNodeUpdates(updates) =>
          val oldTxIds = updates.observedTxs.keys.foldLeft(List.empty[ExchangeTransaction.Id]) {
            case (exclude, txId) =>
              val isNew = observedTxIds.append(txId)
              val updatedExclude = if (isNew) exclude else txId :: exclude
              updatedExclude
          }

          updates.copy(observedTxs = updates.observedTxs -- oldTxIds).updatesByAddresses.foreach {
            case (address, (balanceUpdates, observedTxs)) =>
              val markObservedCommand =
                if (observedTxs.isEmpty) none
                else {
                  val txsWithSpending = observedTxs
                    .view
                    .flatMap { case (id, xs) =>
                      for {
                        txWithChanges <- updates.observedTxs.get(id)
                        observedTxData <-
                          txWithChanges.tx
                            .getOrdersVanilla
                            .leftMap(err => context.log.error(s"tx parsing error $err for tx ${txWithChanges.txId}"))
                            .map(ObservedTxData(_, PositiveMap(xs.view.mapValues(-_).toMap)))
                            .toOption
                      } yield id -> observedTxData
                    }
                    .toMap

                  AddressActor.Command.MarkTxsObserved(txsWithSpending).some
                }
              val changeBalanceCommand = if (balanceUpdates.isEmpty) none else AddressActor.Command.ChangeBalances(balanceUpdates).some
              val message = (markObservedCommand, changeBalanceCommand) match {
                case (Some(a), Some(b)) => AddressActor.Command.ApplyBatch(a, b).some
                case (a, b) => a.orElse(b)
              }
              message.foreach(addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(address, _))
          }

          default()

        case command: Command.ApplyObservedByBroadcaster =>
          val txId = command.tx.id()
          val added = observedTxIds.append(txId)
          if (added) {
            command.tx.traders.foreach { address =>
              val orders = List(command.tx.buyOrder, command.tx.sellOrder)
              val observedTxData = ObservedTxData(orders, command.addressSpending.getOrElse(address, PositiveMap.empty))
              val cmd = AddressActor.Command.MarkTxsObserved(Map(txId -> observedTxData))
              addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(address, cmd)
            }
            default()
          } else Behaviors.same

        case Command.ProcessError(event) =>
          addressDirectoryRef ! event
          Behaviors.same
      }
    }

    default()
  }

}
