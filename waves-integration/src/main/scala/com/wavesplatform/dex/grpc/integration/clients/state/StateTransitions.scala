package com.wavesplatform.dex.grpc.integration.clients.state

import cats.kernel.Semigroup
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.collection.MapOps.Ops2
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainEvent._
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainState._

import scala.collection.immutable.Queue

/**
 * The extension guarantees:
 * 1. During initialization: events will be sent after initial blocks
 * 2. Events has causal ordering
 */

// def recover(state): State = {}
object StateTransitions {

  case class RequestAddressData(address: Address, assets: Set[Asset])

  case class Update(newState: BlockchainState, pushNext: Option[BlockchainBalance], request: List[RequestAddressData])

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

  def apply(origState: BlockchainState, event: BlockchainEvent): Update = origState match {
    case origState: Normal =>
      event match {
        case AppendBlock(block) => Update(origState.withBlock(block), block.changes.some, List.empty)
        case AppendMicroBlock(microBlock) => Update(origState.withMicroBlock(microBlock), microBlock.changes.some, List.empty)
        case RollbackTo(commonBlockInfo) => Update(rollback(origState, commonBlockInfo), none, List.empty)
        case RollbackMicroBlocks(commonBlockInfo) =>
          Update(
            newState = TransientRollbackMicro(
              commonBlockInfo = commonBlockInfo,
              orig = origState,
              liquidBlocks = List.empty
            ),
            pushNext = none,
            request = List.empty
          )
        case _ => Update(origState, none, List.empty) // Won't happen
      }

    case origState: TransientRollback =>
      event match {
        case AppendBlock(block) =>
          val updatedFork = origState.fork.withBlock(block)
          val previousFork = origState.orig.history
          (previousFork.latestBlock, block) match {
            case (Some(previousForkLatestBlock), newForkLatestBlock) =>
              if (newForkLatestBlock.blockInfo.height < previousForkLatestBlock.blockInfo.height)
                Update(origState.copy(fork = updatedFork), none, List.empty)
              else {
                // + liquid
                val changedOnPreviousFork =
                  previousFork.blocksFrom(origState.commonBlockInfo).foldLeft(Set.empty[Address]) {
                    case (r, x) => r ++ x.changedAddresses
                  } ++
                  origState.orig.latestLiquidBlock.flatMap(_.latestBlock).fold(Set.empty[Address])(_.changedAddresses)

                val changedOnNewFork =
                  updatedFork.blocksFrom(origState.commonBlockInfo).foldLeft(Set.empty[Address]) {
                    case (r, x) => r ++ x.changedAddresses
                  }

                val toInvalidate = changedOnPreviousFork -- changedOnNewFork
                val toPush = changedOnNewFork.foldLeft(DataUpdate(Map.empty, Map.empty)) { case (r, address) =>
                  if (toInvalidate.contains(address)) r
                  else {
                    updatedFork.regularBalances.get(address) match {
                      case Some(updatedBalances) =>
                      case None =>
                    }
                    if (origState.orig.history.regularBalances.get(address)) {}
                  }
                }

                Update(
                  newState = TransientResolving(
                    // Note, this state contains not coherent data, because we waiting an information for some addresses
                    orig = updatedFork.copy(
                      regularBalances = origState.orig.history.regularBalances.deepReplace(updatedFork.regularBalances),
                      outLeases = origState.orig.history.outLeases ++ updatedFork.outLeases
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

        // case AppendMicro() => ???
        case RollbackTo(commonBlockInfo) => Update(rollback(Normal(origState.orig, List.empty), commonBlockInfo), none, Set.empty) // ???
        case _ => Update(origState, none, Set.empty) // Won't happen
      }

    case origState: TransientResolving =>
      event match {
        case BalanceUpdates(regularBalances, outLeases) =>
          val init = Update(
            newState = Normal(
              origState.orig.copy(
                regularBalances = origState.orig.regularBalances.deepReplace(regularBalances),
                outLeases = origState.orig.outLeases ++ outLeases
              ),
              liquidBlocks = List.empty
            ),
            pushNext = origState.orig.changes.some,
            request = Set.empty
          )

          origState.stash.foldLeft(init) { case (r, x) =>
            r |+| apply(r.newState, x)
          }

        case _ => Update(origState.copy(stash = origState.stash.enqueue(event)), none, Set.empty)
      }
  }

  def rollback(orig: Normal, commonBlockInfo: BlockRef): BlockchainState = {
    val commonBlocks = orig.history.history.dropWhile(commonBlockInfo.height < _.blockInfo.height)
    TransientRollback(
      commonBlockInfo = commonBlockInfo,
      orig = orig,
      fork = HistoricalData(
        history = commonBlocks,
        regularBalances = Map.empty, // will be updated after fork resolving
        outLeases = Map.empty // will be updated after fork resolving
      )
    )
  }

}
