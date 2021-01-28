package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.domain._
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}

import scala.collection.View

object BlockchainUpdatesConversions {
  type Balances = Map[Address, Map[Asset, Long]]
  type Leases = Map[Address, Long]

  /**
   * Cases:
   * 1. Downloading blocks: Append+
   * 2. Appending on a network's height: AppendMicro*, RollbackMicro?, Append
   * 2. Rollback: Rollback, Append+
   */
  def toEvent(event: BlockchainUpdated): Option[WavesNodeEvent] = {
    val blockRef = BlockRef(event.height, event.id.toVanilla)
    event.update match {
      case Update.Empty => none // Nothing to do
      case Update.Append(updates) =>
        val regularBalanceChanges = combineBalanceUpdates(updates.stateUpdate.view ++ updates.transactionStateUpdates)
        val outgoingLeasingChanges = combineLeaseUpdates(updates.stateUpdate.view ++ updates.transactionStateUpdates)

        // block.transactions, updates.transactionIds, updates.transactionStateUpdates have the same order
        val blockInfo = updates.body match {
          case Body.Empty => none
          case Body.Block(block) =>
            val x = block.block.get
            (WavesBlock.Type.FullBlock, x.header.get.reference.toVanilla, x.transactions).some
          case Body.MicroBlock(block) =>
            val x = block.microBlock.get.microBlock.get
            (WavesBlock.Type.MicroBlock, x.reference.toVanilla, x.transactions).some
        }

        blockInfo.map { case (tpe, reference, transactionBodies) =>
          val block = WavesBlock(
            ref = blockRef,
            reference = reference,
            changes = BlockchainBalance(regularBalanceChanges, outgoingLeasingChanges),
            tpe = tpe,
            confirmedTxs = updates.transactionIds.view
              .zip(updates.transactionStateUpdates)
              .zip(transactionBodies)
              .map {
                case ((id, update), tx) => id -> TransactionWithChanges(id, tx, update)
              }
              .toMap
          )

          WavesNodeEvent.Appended(block)
        }

      case Update.Rollback(value) =>
        value.`type` match {
          case RollbackType.BLOCK | RollbackType.MICROBLOCK =>
            WavesNodeEvent.RolledBack(WavesNodeEvent.RolledBack.To.CommonBlockRef(blockRef)).some
          case _: RollbackType.Unrecognized => none
        }
    }
  }

  def combineBalanceUpdates(stateUpdate: View[StateUpdate]): Balances =
    stateUpdate.flatMap(_.balances).foldLeft[Balances](Map.empty) {
      case (r, x) =>
        x.amount.fold(r) { assetAmount =>
          val address = x.address.toVanillaAddress
          r.updated(
            address,
            r
              .getOrElse(address, Map.empty)
              .updated(assetAmount.assetId.toVanillaAsset, assetAmount.amount)
          )
        }
    }

  def combineLeaseUpdates(stateUpdate: View[StateUpdate]): Leases =
    stateUpdate.flatMap(_.leases).map(x => x.address.toVanillaAddress -> x.out).foldLeft[Leases](Map.empty)(_ + _)

}
