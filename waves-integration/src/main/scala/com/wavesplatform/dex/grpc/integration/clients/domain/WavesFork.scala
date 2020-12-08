package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import cats.instances.list._
import cats.instances.tuple._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesFork.Status

// TODO DEX-1009 Unit test
// TODO DEX-1010 Can be connected again if forkChain rolled back behind origChain and restored the same chain
// TODO DEX-1011 This class is too slow for his purposes
case class WavesFork private[domain](origChain: WavesChain, forkChain: WavesChain, connected: Boolean) {

  // TODO DEX-1009 Move to tests in the end

  require(!origChain.isEmpty, "empty origChain")

  private lazy val forkChainLastBlock = forkChain.history.last // Use only if connected!
  private lazy val origChainLastBlock = origChain.history.last

  require(
    // if connected, the original chain contains a last block from the fork
    connected && origChain.history.exists(_.ref == forkChainLastBlock.ref) ||
    // if not, fork happened before the oldest stored block in the original chain
    !connected && forkChain.history.lastOption.fold(forkChain.height)(_.ref.height) <= origChainLastBlock.ref.height,
    "origChain, forkChain and connected invariant"
  )

  // TODO DEX-1009 An additional invariant: forkChain should contain only one common block!

  def height: Int = forkChain.height

  // TODO DEX-1010 if we have e.g. 10 blocks and did rollback for 2, we don't need to request balances. Check this case
  def withBlock(block: WavesBlock): Status =
    forkChain.withBlock(block) match {
      case Left(e) => Status.Failed(withoutLast, e)
      case Right(updatedforkChain) =>
        // TODO DEX-1010 if we are restoring the origChain in forkChain, hold NotResolved until we get all micro blocks
        // Compare heights to solve a situation when there no transactions in the network since some height
        if (block.tpe == WavesBlock.Type.FullBlock && block.ref.height < origChain.height)
          Status.NotResolved(copy(forkChain = updatedforkChain))
        else {
          val (origDroppedBlocks, updatedForkDroppedBlocks, activeChain) =
            if (connected) {
              // It's okay to use forkChainLastBlock here, because a micro block added to the forkChain without other changes
              val (commonChain, droppedBlocks) = origChain.dropAfter(forkChainLastBlock.ref)
              (
                droppedBlocks,
                updatedforkChain.history.init,
                updatedforkChain.copy(history = updatedforkChain.history ::: commonChain.history.tail)
              )
            } else (origChain.history, updatedforkChain.history, updatedforkChain)

          val origForkDiffIndex = origDroppedBlocks.foldMap(_.diffIndex)
          val (updatedForkAllChanges, updatedForkDiffIndex) = updatedForkDroppedBlocks
            .foldLeft(Monoid.empty[(BlockchainBalance, DiffIndex)]) {
              case (r, block) => (block.changes, block.diffIndex) |+| r
            }

          Status.Resolved(
            activeChain = activeChain,
            newChanges = updatedForkAllChanges, // TODO DEX-1011 Probably we can filter out this, but it is done on next layer. Should we do?
            lostDiffIndex = origForkDiffIndex.without(updatedForkDiffIndex)
          )
        }
    }

  def withoutLast: WavesFork = mkFromUpdatedforkChain(forkChain.withoutLast)

  def rollBackTo(height: Int): WavesFork = mkFromUpdatedforkChain(forkChain.dropAfter(height)._1)
  def rollBackTo(ref: BlockRef): WavesFork = mkFromUpdatedforkChainRef(forkChain.dropAfter(ref)._1, ref)

  // TODO DEX-1009
  // Valid only when we remove blocks
  private def mkFromUpdatedforkChain(updatedforkChain: WavesChain): WavesFork =
    // If there is no blocks in withoutLastLiquid
    // * we take one from the origChain to preserve connected
    // * or disconnect them if origChain doesn't contain a common ancestor
    if (connected && updatedforkChain.isEmpty)
      origChain.history.find(x => x.tpe == WavesBlock.Type.FullBlock && x.ref.height == updatedforkChain.height) match {
        case None => copy(forkChain = updatedforkChain, connected = false)
        case Some(newCommonBlock) => copy(forkChain = WavesChain(List(newCommonBlock), newCommonBlock.ref.height))
      }
    else copy(forkChain = updatedforkChain)

  // TODO DEX-1009
  // Valid only when we remove blocks
  private def mkFromUpdatedforkChainRef(updatedforkChain: WavesChain, commonBlockRef: BlockRef): WavesFork =
    // If there is no blocks in withoutLastLiquid
    // * we take one from the origChain to preserve connected
    // * or disconnect them if origChain doesn't contain a common ancestor
    if (connected && updatedforkChain.isEmpty)
      origChain.history.find(_.ref == commonBlockRef) match {
        case None => copy(forkChain = updatedforkChain, connected = false)
        case Some(newCommonBlock) => copy(forkChain = WavesChain(List(newCommonBlock), commonBlockRef.height))
      }
    else copy(forkChain = updatedforkChain)

  override def toString: String = s"WavesFork(o=$origChain, f=$forkChain, connected=$connected)"
}

object WavesFork {

  def mk(origChain: WavesChain, commonBlockRef: BlockRef): WavesFork = mkFromCommonChain(origChain, origChain.dropAfter(commonBlockRef)._1)
  def mk(origChain: WavesChain, commonHeight: Int): WavesFork = mkFromCommonChain(origChain, origChain.dropAfter(commonHeight)._1)

  def mkRolledBackByOne(origChain: WavesChain): WavesFork =
    mkFromCommonChain(origChain, origChain.withoutLast) // Or better use WavesFork.withoutLast

  private def mkFromCommonChain(origChain: WavesChain, commonChain: WavesChain): WavesFork =
    WavesFork(origChain, commonChain.copy(history = commonChain.history.headOption.toList), !commonChain.isEmpty)

  sealed trait Status extends Product with Serializable

  object Status {
    case class Resolved(activeChain: WavesChain, newChanges: BlockchainBalance, lostDiffIndex: DiffIndex) extends Status
    case class NotResolved(updatedFork: WavesFork) extends Status
    case class Failed(updatedFork: WavesFork, reason: String) extends Status
  }

}
