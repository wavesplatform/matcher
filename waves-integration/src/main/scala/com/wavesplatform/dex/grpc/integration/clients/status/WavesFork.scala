package com.wavesplatform.dex.grpc.integration.clients.status

import cats.Monoid
import cats.instances.list._
import cats.instances.tuple._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.grpc.integration.clients.status.WavesFork.Status

// TODO test
// TODO this class is too slow for his purposes
// invariant:
//  if forkBranch is not empty, forkBranch.last contains in
case class WavesFork private[status] (origBranch: WavesBranch, forkBranch: WavesBranch, connected: Boolean) {

  // TODO move to tests in the end

  require(!origBranch.isEmpty, "empty origBranch")

  private lazy val forkBranchLastBlock = forkBranch.history.last // Use only if connected!
  private lazy val origBranchLastBlock = origBranch.history.last

  require(
    // if connected, the original branch contains a last block from the fork
    connected && origBranch.history.exists(_.ref == forkBranchLastBlock.ref) ||
    // if not, fork happened before the oldest stored block in the original branch
    !connected && forkBranch.history.lastOption.fold(forkBranch.height)(_.ref.height) <= origBranchLastBlock.ref.height,
    "origBranch, forkBranch and connected invariant"
  )

  // TODO An additional invariant: forkBranch should contain only one common block!

  def height: Int = forkBranch.height

  def withBlock(block: WavesBlock): Status =
    forkBranch.withBlock(block) match {
      case Left(e) => Status.Failed(withoutLastLiquid, e)
      case Right((_, updatedForkBranch)) =>
        // TODO if we are restoring the origBranch in forkBranch, hold NotResolved until we get all micro blocks
        if (block.tpe == WavesBlock.Type.FullBlock) Status.NotResolved(copy(forkBranch = updatedForkBranch))
        else {
          val (origDroppedBlocks, updatedForkDroppedBlocks, activeBranch) =
            if (connected) {
              // It's okay to use forkBranchLastBlock here, because a micro block added to the forkBranch without other changes
              val (commonBranch, droppedBlocks) = origBranch.dropAfter(forkBranchLastBlock.ref)
              (droppedBlocks, updatedForkBranch.history.init, updatedForkBranch.copy(history = updatedForkBranch.history ::: commonBranch.history.tail))
            } else (origBranch.history, updatedForkBranch.history, updatedForkBranch) // MicroBlock && !connected

          val origForkDiffIndex = origDroppedBlocks.foldMap(_.diffIndex)
          val (updatedForkAllChanges, updatedForkDiffIndex) = updatedForkDroppedBlocks
            .foldLeft(Monoid.empty[(BlockchainBalance, DiffIndex)]) {
              case (r, block) => (block.changes, block.diffIndex) |+| r
            }

          Status.Resolved(
            activeBranch = activeBranch,
            newChanges = updatedForkAllChanges, // TODO Probably we can filter out this, but it is done on next layer. Should we do?
            lostDiffIndex = origForkDiffIndex.without(updatedForkDiffIndex)
          )
        }
    }

  def withoutLastLiquid: WavesFork = mkFromUpdatedForkBranch(forkBranch.withoutLastLiquid, forkBranch.height - 1)

  def rollBackTo(height: Int): WavesFork = mkFromUpdatedForkBranch(forkBranch.dropAfter(height)._1, height)
  def rollBackTo(ref: BlockRef): WavesFork = mkFromUpdatedForkBranch(forkBranch.dropAfter(ref)._1, height)

  private def mkFromUpdatedForkBranch(updatedForkBranch: WavesBranch, newHeight: Int): WavesFork =
    // If there is no blocks in withoutLastLiquid
    // * we take one from the origBranch to preserve connected
    // * or disconnect them if origBranch doesn't contain a common ancestor
    if (updatedForkBranch.isEmpty) {
      val newCommonBlock = if (connected) origBranch.dropAfter(forkBranchLastBlock.ref)._2.headOption else None
      copy(forkBranch = WavesBranch(newCommonBlock.toList, newHeight), connected = newCommonBlock.nonEmpty)
    } else copy(forkBranch = updatedForkBranch)

}

object WavesFork {

  def mk(origBranch: WavesBranch, commonBlockRef: BlockRef): WavesFork = mkFromCommonBranch(origBranch, origBranch.dropAfter(commonBlockRef)._1)
  def mk(origBranch: WavesBranch, commonHeight: Int): WavesFork = mkFromCommonBranch(origBranch, origBranch.dropAfter(commonHeight)._1)

  def mkRolledBackByOne(origBranch: WavesBranch): WavesFork =
    mkFromCommonBranch(origBranch, origBranch.withoutLastLiquid) // Or better use WavesFork.withoutLastLiquid

  def mkFromForkBranch(origBranch: WavesBranch, forkBranch: WavesBranch): WavesFork =
    WavesFork(origBranch, forkBranch, forkBranch.history.lastOption.exists(x => origBranch.history.exists(_.ref == x.ref)))

  private def mkFromCommonBranch(origBranch: WavesBranch, commonBranch: WavesBranch): WavesFork =
    WavesFork(origBranch, commonBranch.copy(history = commonBranch.history.headOption.toList), !commonBranch.isEmpty)

  sealed trait Status extends Product with Serializable

  object Status {
    case class Resolved(activeBranch: WavesBranch, newChanges: BlockchainBalance, lostDiffIndex: DiffIndex) extends Status
    case class NotResolved(updatedFork: WavesFork) extends Status
    case class Failed(updatedFork: WavesFork, reason: String) extends Status
  }

}
