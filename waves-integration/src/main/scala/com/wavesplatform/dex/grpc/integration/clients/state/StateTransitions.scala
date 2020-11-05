package com.wavesplatform.dex.grpc.integration.clients.state

import cats.kernel.Semigroup
import cats.syntax.option._
import cats.instances.map._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.collection.MapOps.Ops2
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainEvent._
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainStatus._
import com.wavesplatform.dex.grpc.integration.clients.state.WavesFork.DropResult

import scala.collection.immutable.Queue

/**
 * The extension guarantees:
 * 1. During initialization: events will be sent after initial blocks
 * 2. Events has causal ordering
 */

// def recover(state): State = {}
object StateTransitions {

  case class RequestAddressData(address: Address, assets: Set[Asset])

  // TODO replace with interface with methods?
  case class Update(newState: BlockchainStatus, pushNext: Option[BlockchainBalance], request: List[RequestAddressData])

  object Update {

    implicit val updateSemigroup: Semigroup[Update] = (x: Update, y: Update) =>
      Update(
        newState = y.newState,
        pushNext = (x.pushNext, y.pushNext) match {
          case (Some(x), Some(y)) => x.combine(y).some
          case (x, y) => x.orElse(y)
        },
        request = x.request | y.request
      )

  }

  def apply(origState: BlockchainStatus, event: BlockchainEvent): Update = origState match {
    case origState: Normal =>
      event match {
        case AppendBlock(block) => Update(origState.mainFork.withBlock(block), block.changes.some, List.empty)
        case AppendMicroBlock(microBlock) => Update(origState.mainFork.withMicroBlock(microBlock), microBlock.changes.some, List.empty)
        case RollbackTo(commonBlockRef) =>
          origState.mainFork.dropAfter(commonBlockRef) match {
            case DropResult.Succeeded(newFork) =>
              TransientRollback(
                commonBlockRef = commonBlockRef,
                mainFork = origState.mainFork,
                newFork = newFork
              )
            case DropResult.DroppedAll => Update(Init, none, List.empty) // TODO Request all
            case DropResult.UnknownLiquidBlockRef(knownFork) => Update(Normal(knownFork), none, List.empty) // TODO Request from height, Transient somewhat to check block id?
          }
        case _ => Update(origState, none, List.empty) // Won't happen TODO process?
      }

    case origState: TransientRollback =>
      event match {
        case AppendBlock(block) =>
          val updatedNewFork = origState.newFork.withBlock(block)
          val previousFork = origState.mainFork
          if (updatedNewFork.history.head.ref.height < previousFork.history.head.ref.height)
            Update(origState.copy(newFork = updatedNewFork), none, List.empty)
          else {
            val changedOnPreviousFork =
              previousFork.blocksFrom(origState.commonBlockRef).combinedBlockchainBalance |+|
              origState.mainFork.latestLiquidBlock.changes

            val changedOnNewFork = updatedNewFork.blocksFrom(origState.commonBlockRef).combinedBlockchainBalance

            val toInvalidate = changedOnPreviousFork -- changedOnNewFork

            val toPush = changedOnNewFork.foldLeft(DataUpdate(Map.empty, Map.empty)) { case (r, address) =>
              if (toInvalidate.contains(address)) r
              else {
                updatedNewFork.regularBalances.get(address) match {
                  case Some(updatedBalances) =>
                  case None =>
                }
                if (origState.mainFork.mainFork.regularBalances.get(address)) {}
              }
            }

            Update(
              newState = TransientResolving(
                // Note, this state contains not coherent data, because we waiting an information for some addresses
                mainFork = updatedNewFork.copy(
                  regularBalances = origState.mainFork.mainFork.regularBalances.deepReplace(updatedNewFork.regularBalances),
                  outLeases = origState.mainFork.mainFork.outLeases ++ updatedNewFork.outLeases
                ),
                waitInfoFor = toInvalidate,
                stash = Queue.empty
              ),
              pushNext = none, // ???
              request = toInvalidate
            )
          }

         case _ => Update(origState, none, Set.empty) // Won't happen
      }

    case origState: TransientResolving =>
      event match {
        case BalanceUpdates(regularBalances, outLeases) =>
          val init = Update(
            newState = Normal(
              origState.mainFork.copy(
                regularBalances = origState.mainFork.regularBalances.deepReplace(regularBalances),
                outLeases = origState.mainFork.outLeases ++ outLeases
              ),
              liquidBlocks = List.empty
            ),
            pushNext = origState.mainFork.changes.some,
            request = Set.empty
          )

          origState.stash.foldLeft(init) { case (r, x) =>
            r |+| apply(r.newState, x)
          }

        case _ => Update(origState.copy(stash = origState.stash.enqueue(event)), none, Set.empty)
      }
  }

  def rollback(mainFork: WavesFork, commonBlockRef: BlockRef): Option[BlockchainStatus] = mainFork.dropAfter(commonBlockRef).map { newFork =>
    TransientRollback(
      commonBlockRef = commonBlockInfo,
      mainFork = mainFork,
      newFork = newFork
    )

}
