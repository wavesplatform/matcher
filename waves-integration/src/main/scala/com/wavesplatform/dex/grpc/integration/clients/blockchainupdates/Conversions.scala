package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.domain.{BlockRef, BlockchainBalance, WavesBlock, WavesNodeEvent}
import com.wavesplatform.dex.grpc.integration.protobuf.PbToDexConversions._
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}

import scala.collection.View

object Conversions {
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
        val outLeasesChanges = combineLeaseUpdates(updates.stateUpdate.view ++ updates.transactionStateUpdates)

        val blockInfo = updates.body match {
          case Body.Empty => none
          case Body.Block(block) => (WavesBlock.Type.FullBlock, block.block.get.header.get.reference.toVanilla).some
          case Body.MicroBlock(block) => (WavesBlock.Type.MicroBlock, block.microBlock.get.microBlock.get.reference.toVanilla).some
        }

        blockInfo.map { case (tpe, reference) =>
          val block = WavesBlock(
            ref = blockRef,
            reference = reference,
            changes = BlockchainBalance(regularBalanceChanges, outLeasesChanges),
            tpe = tpe
          )

          WavesNodeEvent.Appended(block, updates.transactionIds)
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
