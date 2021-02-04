package com.wavesplatform.dex.actors.events

import akka.actor.typed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.{actor => classic}
import cats.instances.long._
import cats.instances.map._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.actors.address.{AddressActor, AddressDirectoryActor}
import com.wavesplatform.dex.actors.tx.ExchangeTransactionBroadcastActor.{Observed, Command => Broadcaster}
import com.wavesplatform.dex.collections.{FifoSet, PositiveMap}
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
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
    case class ApplyObservedByBroadcaster(tx: ExchangeTransaction, addressSpending: Map[Address, PositiveMap[Asset, Long]]) extends Command
  }

  def apply(
    addressDirectoryRef: classic.ActorRef,
    dbWriterRef: classic.ActorRef,
    broadcasterRef: typed.ActorRef[Broadcaster],
    createTransaction: CreateTransaction
  ): Behavior[Message] = Behaviors.setup { context =>
    val broadcastAdapter: ActorRef[Observed] = context.messageAdapter[Observed] {
      case Observed(tx, addressSpending) => Command.ApplyObservedByBroadcaster(tx, addressSpending)
    }

    // TODO think about initialized and non-initialized AA for knownTxIds
    def default(observedTxIds: FifoSet[ExchangeTransaction.Id]): Behaviors.Receive[Message] = Behaviors.receive[Message] { (context, message) =>
      message match {
        case Command.Process(event) =>
          event match {
            case event: Events.OrderAdded =>
              addressDirectoryRef ! AddressActor.Command.ApplyOrderBookAdded(event)
              Behaviors.same

            case event: Events.OrderExecuted =>
              // If we here, AddressActor is guaranteed to be created, because this happens only after Events.OrderAdded
              val expectedTx = createTransaction(event) match {
                case Right(tx) =>
                  val txCreated = ExchangeTransactionCreated(tx)
                  context.log.info(s"Created ${tx.json()}")
                  dbWriterRef ! txCreated

                  val addressSpendings =
                    Map(event.counter.order.sender.toAddress -> PositiveMap(event.counterExecutedSpending)) |+|
                    Map(event.submitted.order.sender.toAddress -> PositiveMap(event.submittedExecutedSpending))

                  broadcasterRef ! Broadcaster.Broadcast(broadcastAdapter, addressSpendings, tx)
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
              addressDirectoryRef ! AddressActor.Command.ApplyOrderBookExecuted(event, expectedTx)
              Behaviors.same // We don't update "observedTxIds" here, because expectedTx relates to "createdTxs"

            case event: Events.OrderCanceled =>
              // If we here, AddressActor is guaranteed to be created, because this happens only after Events.OrderAdded
              addressDirectoryRef ! AddressActor.Command.ApplyOrderBookCanceled(event)
              Behaviors.same
          }

        case Command.ApplyNodeUpdates(updates) =>
          val (updatedKnownTxIds, oldTxIds) = updates.observedTxs.keys.foldLeft((observedTxIds, List.empty[ExchangeTransaction.Id])) {
            case ((knownTxIds, exclude), txId) =>
              val (updatedKnownTxIds, isNew) = knownTxIds.append(txId)
              val updatedExclude = if (isNew) exclude else txId :: exclude
              (updatedKnownTxIds, updatedExclude)
          }

          updates.copy(observedTxs = updates.observedTxs -- oldTxIds).updatesByAddresses.foreach {
            case (address, (balanceUpdates, observedTxs)) =>
              val markObservedCommand =
                if (observedTxs.isEmpty) none
                else {
                  val xs = observedTxs.view.mapValues(xs => PositiveMap(xs.view.mapValues(-_).toMap)).toMap
                  AddressActor.Command.MarkTxsObserved(xs).some
                }
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
          val (updatedKnownTxIds, added) = observedTxIds.append(txId)
          if (added) {
            command.tx.traders.foreach { address =>
              val x = AddressActor.Command.MarkTxsObserved(Map(txId -> command.addressSpending.getOrElse(address, PositiveMap.empty)))
              addressDirectoryRef ! AddressDirectoryActor.Command.ForwardMessage(address, x)
            }
            default(updatedKnownTxIds)
          } else Behaviors.same

        case Command.ProcessError(event) =>
          addressDirectoryRef ! event
          Behaviors.same
      }
    }

    // According to https://github.com/wavesplatform/protobuf-schemas/blob/master/proto/waves/transaction.proto
    // Transaction
    //   chain_id: 4 +
    //   sender_public_key: 32 +
    //   fee: Amount
    //     asset_id: 0 +
    //     amount: 8 +
    //   timestamp: 8 +
    //   version: 4 +
    //   exchange: ExchangeTransactionData
    //	   amount: 8 +
    // 	   price: 8 +
    //     buy_matcher_fee: 8 +
    //     sell_matcher_fee: 8 +
    //     orders: Order: 2 *
    //	     chain_id: 4 +
    //       sender_public_key: 32 +
    //       matcher_public_key: 32 +
    //       asset_pair: AssetPair:
    //         amount_asset_id: 0 +
    //         price_asset_id: 0 +
    //       order_side: 1 +
    //       amount: 8 +
    //       price: 8 +
    //       timestamp: 8 +
    //       expiration: 8 +
    //       matcher_fee: 0 + 8 +
    //       version: 4 +
    //       proofs: 32
    // The minimal size of exchange transaction of v3 is 376 bytes =
    //   4 + 32 + 0 + 8 + 8 + 4 + 8 + 8 + 8 + 8 + 2 * (4 + 32 + 32 + 0 + 0 + 1 + 8 + 8 + 8 + 8 + 0 + 8 + 3 + 32)
    //
    // According to https://docs.waves.tech/en/blockchain/block/
    // The maximum size of a block is 1MB = 1048576 bytes
    //
    // Thus ≈ 2789 exchange transactions fit into a block.
    // 2 blocks for this FifoSet is enough, because it is auxiliary functionality.
    default(FifoSet.limited[ExchangeTransaction.Id](6000))
  }

}
