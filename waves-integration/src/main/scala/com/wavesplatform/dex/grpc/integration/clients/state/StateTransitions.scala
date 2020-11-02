//package com.wavesplatform.dex.grpc.integration.clients.state
//
//import cats.kernel.Semigroup
//import cats.syntax.option._
//import cats.syntax.semigroup._
//import com.wavesplatform.dex.collection.ListOps.Ops
//import com.wavesplatform.dex.collection.MapOps.Ops
//import com.wavesplatform.dex.domain.account.Address
//import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainEvent._
//import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainState._
//
//import scala.collection.immutable.Queue
//
///**
// * The extension guarantees:
// * 1. During initialization: events will be sent after initial blocks
// * 2. Events has causal ordering
// */
//
//// def recover(state): State = {}
//object StateTransitions {
//
//  case class Update(newState: BlockchainState, pushNext: Option[DataUpdate], request: Set[Address])
//
//  object Update {
//
//    implicit val updateSemigroup: Semigroup[Update] = (x: Update, y: Update) =>
//      Update(
//        newState = y.newState,
//        pushNext = (x.pushNext, y.pushNext) match {
//          case (Some(x), Some(y)) => x.combine(y).some
//          case (x, y) => x.orElse(y)
//        },
//        request = x.request | y.request
//      )
//
//  }
//
//  def apply(origState: BlockchainState, event: BlockchainEvent): Update = origState match {
//    case origState: Normal =>
//      event match {
//        case Append(changes) =>
//          // Combine changes: a previous block, the freshest liquid block, the new block changes
//          val updatedData = origState
//            .liquidBlocks.headOption.foldLeft(changes :: Nil)((r, x) => x :: r)
//            .foldLeft(origState.data)(_.unsafeCombine(_))
//
//          // Reset liquid blocks, because we added a new block
//          Update(Normal(updatedData, List.empty), changes.changes.some, Set.empty)
//
//        case AppendMicro(changes) =>
//          val newLiquidBlock = origState
//            .liquidBlocks.headOption.foldLeft(changes)(_.unsafeCombine(_))
//
//          Update(Normal(origState.data, newLiquidBlock :: origState.liquidBlocks), changes.changes.some, Set.empty)
//
//        case Rollback(commonBlockInfo) => Update(rollback(origState, commonBlockInfo), none, Set.empty)
//
//        case RollbackMicro(commonBlockInfo) =>
//          val actualLiquidBlocks =
//            if (commonBlockInfo.id == origState.blockInfo.id) List.empty
//            else {
//              val r = origState.liquidBlocks.dropWhile(_.blockInfo.id != commonBlockInfo.id)
//              if (r.isEmpty) throw new RuntimeException("Huh")
//              r
//            }
//
//          // TODO cleanup
//          val toInvalidate = origState.liquidBlocks.headOption.flatMap(_.changedAddresses.headOption).fold(Set.empty[Address])(_.addresses) --
//            actualLiquidBlocks.headOption.flatMap(_.changedAddresses.headOption).fold(Set.empty[Address])(_.addresses)
//
//          Update(
//            newState = rollback(Normal(origState.data, List.empty), commonBlockInfo),
//            pushNext = none,
//            request = toInvalidate
//          )
//
//        case _ => Update(origState, none, Set.empty) // Won't happen
//      }
//
//    case origState: TransientRollback =>
//      event match {
//        case Append(changes) =>
//          val updatedAccumulated = origState.accumulated.combine(changes)
//          if (updatedAccumulated.blockInfo.height == origState.orig.blockInfo.height) {
//            val origBlockchainData = origState.orig
//            val (fork, common) = origBlockchainData.data.changedAddresses.splitOnCondUnordered(origState.commonBlockInfo.height <= _.height)
//            val toInvalidate = fork.view.flatMap(_.addresses).toSet
//
//            // Note, this state contains not coherent data, because we waiting an information for some addresses
//            val updatedData = BlockchainData(
//              blockInfo = updatedAccumulated.blockInfo,
//              changedAddresses = updatedAccumulated.changedAddresses ::: common,
//              regularBalances = origBlockchainData.data.regularBalances.deepReplace(updatedAccumulated.regularBalances),
//              outLeases = origBlockchainData.data.outLeases ++ updatedAccumulated.outLeases
//            )
//
//            val nextState = TransientResolving(
//              orig = updatedData,
//              waitInfoFor = toInvalidate -- updatedAccumulated.changedAddresses.flatMap(_.addresses),
//              stash = Queue.empty
//            )
//
//            Update(nextState, none, toInvalidate)
//          } else Update(origState.copy(accumulated = updatedAccumulated), none, Set.empty)
//
//        // case AppendMicro() => ???
//        case Rollback(commonBlockInfo) => Update(rollback(Normal(origState.orig, List.empty), commonBlockInfo), none, Set.empty) // ???
//        case _ => Update(origState, none, Set.empty) // Won't happen
//      }
//
//    case origState: TransientResolving =>
//      event match {
//        case DataUpdate(regularBalances, outLeases) =>
//          val init = Update(
//            newState = Normal(
//              origState.orig.copy(
//                regularBalances = origState.orig.regularBalances.deepReplace(regularBalances),
//                outLeases = origState.orig.outLeases ++ outLeases
//              ),
//              liquidBlocks = List.empty
//            ),
//            pushNext = origState.orig.changes.some,
//            request = Set.empty
//          )
//
//          origState.stash.foldLeft(init) { case (r, x) =>
//            r |+| apply(r.newState, x)
//          }
//
//        case _ => Update(origState.copy(stash = origState.stash.enqueue(event)), none, Set.empty)
//      }
//  }
//
//  def rollback(orig: Normal, commonBlockInfo: BlockInfo): BlockchainState =
//    TransientRollback(
//      commonBlockInfo = commonBlockInfo,
//      orig = orig,
//      accumulated = BlockchainData(
//        blockInfo = commonBlockInfo,
//        changedAddresses = List.empty,
//        regularBalances = Map.empty,
//        outLeases = Map.empty
//      )
//    )
//
//}
