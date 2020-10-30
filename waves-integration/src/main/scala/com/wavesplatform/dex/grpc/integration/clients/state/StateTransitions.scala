package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.collection.MapOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainEvent._
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainState._
import cats.syntax.option._

import scala.collection.immutable.Queue

/**
 * The extension guarantees:
 * 1. During initialization: events will be sent after initial blocks
 * 2. Events has causal ordering
 */

// def recover(state): State = {}
object StateTransitions {

  def apply(origState: BlockchainState, event: BlockchainEvent): (BlockchainState, Option[BlockchainData]) = origState match {
    case origState: Normal =>
      event match {
        case Append(changes) => (Normal(origState.data.combine(changes)), changes.some)
        case Rollback(commonBlockInfo) => (rollback(origState, commonBlockInfo), none)
        case _ => (origState, none) // Won't happen
      }

    case origState: TransientRollback =>
      event match {
        case Append(changes) =>
          val updatedAccumulated = origState.accumulated.combine(changes)
          if (updatedAccumulated.blockInfo.height == origState.orig.blockInfo.height) {
            val origBlockchainData = origState.orig
            val (fork, common) = origBlockchainData.changedAddresses.splitOnCondUnordered(origState.commonBlockInfo.height <= _.height)
            val toInvalidate = fork.view.flatMap(_.addresses).toSet

            // Note, this state contains not coherent data, because we waiting an information for some addresses
            val updatedData = BlockchainData(
              blockInfo = updatedAccumulated.blockInfo,
              changedAddresses = updatedAccumulated.changedAddresses ::: common,
              regularBalances = origBlockchainData.regularBalances.deepReplace(updatedAccumulated.regularBalances),
              outLeases = origBlockchainData.outLeases ++ updatedAccumulated.outLeases
            )

            val nextState = TransientResolving(
              orig = updatedData,
              waitInfoFor = toInvalidate -- updatedAccumulated.changedAddresses.flatMap(_.addresses),
              stash = Queue.empty
            )

            (nextState, none)
          } else (origState.copy(accumulated = updatedAccumulated), none)

        case Rollback(commonBlockInfo) => (rollback(Normal(origState.orig), commonBlockInfo), none)
        case _ => (origState, none) // Won't happen
      }

    case origState: TransientResolving =>
      event match {
        case DataUpdate(regularBalances, outLeases) =>
          val init = Normal(
            origState.orig.copy(
              regularBalances = origState.orig.regularBalances.deepReplace(regularBalances),
              outLeases = origState.orig.outLeases ++ outLeases
            )
          )

          origState.stash.foldLeft[BlockchainState](init)(apply)

        case _ => (origState.copy(stash = origState.stash.enqueue(event)), none)
      }
  }

  def rollback(orig: Normal, commonBlockInfo: BlockInfo): BlockchainState =
    TransientRollback(
      commonBlockInfo = commonBlockInfo,
      orig = orig.data,
      accumulated = BlockchainData(
        blockInfo = commonBlockInfo,
        changedAddresses = List.empty,
        regularBalances = Map.empty,
        outLeases = Map.empty
      )
    )

}
