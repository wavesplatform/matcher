package com.wavesplatform.dex.grpc.integration.clients.status

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.status.WavesBranch.dropPreviousFork

import scala.annotation.tailrec

/**
 * TODO Rename class or a history field to reflect that there are not all blocks
 * TODO constructor with auto height when passed non empty list
 * @param history Contains micro blocks
 */
case class WavesBranch(history: List[WavesBlock], height: Int) { // TODO cut to last N blocks is required to reduce amount of stored blocks!

  require(history.headOption.map(_.ref.height).forall(_ == height), "height corresponds last block")

  def isEmpty: Boolean = history.isEmpty

  // TODO remove withBlock, because its type creases a possibility that some blocks can be dropped during a micro block appending

  /**
   * @return (droppedMicroBlocks, updatedFork)
   */
  def withBlock(block: WavesBlock): Either[String, (List[WavesBlock], WavesBranch)] =
    if (block.tpe == WavesBlock.Type.FullBlock) withFullBlock(block)
    else withMicroBlock(block).map((List.empty, _))

  /**
   * @return Guarantees WavesFork is not empty
   */
  private def withFullBlock(block: WavesBlock): Either[String, (List[WavesBlock], WavesBranch)] = history match {
    case Nil => (List.empty, WavesBranch(block :: history, block.ref.height)).asRight
    case prev :: _ =>
      if (block.ref.height == prev.ref.height + 1)
        dropPreviousFork(block, history).map {
          case (droppedBlocks, historyAfterCut) =>
            val (microBlocks, blocksHistory) = historyAfterCut.splitOnCondReversed(_.tpe == WavesBlock.Type.MicroBlock)
            val updatedFork =
              if (microBlocks.isEmpty) WavesBranch(block :: blocksHistory, block.ref.height)
              else blocksHistory match {
                case Nil => throw new RuntimeException("Imposibru")
                case keyBlock :: restBlocks =>
                  val liquidBlock = NonEmptyList(keyBlock, microBlocks) // Have the direct order
                  WavesBranch(block :: mkHardenedBlock(liquidBlock) :: restBlocks, block.ref.height)
              }
            (droppedBlocks, updatedFork)
        }
      else s"The new block ${block.ref} (reference=${block.reference}) must be after ${prev.ref}".asLeft
  }

  private def withMicroBlock(microBlock: WavesBlock): Either[String, WavesBranch] = history match {
    case Nil => s"Can't attach a micro block $microBlock to empty chain".asLeft
    case prev :: _ =>
      if (microBlock.ref.height == prev.ref.height && microBlock.reference == prev.ref.id)
        WavesBranch(microBlock :: history, microBlock.ref.height).asRight
      else
        s"The new micro block ${microBlock.ref} (reference=${microBlock.reference}) must reference the last block ${prev.ref}".asLeft
  }

  def diffIndex: DiffIndex = history.foldMap(_.diffIndex)

  def withoutLastLiquid: WavesBranch = {
    val withoutMicroBlocks = history.dropWhile(_.tpe == WavesBlock.Type.MicroBlock)
    WavesBranch(if (withoutMicroBlocks.isEmpty) Nil else withoutMicroBlocks.tail, math.max(0, height - 1))
  }

  /**
   * @return (new branch, dropped blocks), where dropped blocks are ordered from oldest to newest
   */
  def dropAfter(ref: BlockRef): (WavesBranch, List[WavesBlock]) = {
    val (droppedHistory, commonHistory) = history.splitOnCondReversed(x => !(x.ref.height == ref.height && x.ref.id == ref.id))
    (WavesBranch(commonHistory, droppedHistory.headOption.fold(height)(x => math.max(0, x.ref.height - 1))), droppedHistory)
  }

  def dropAfter(height: Int): (WavesBranch, List[WavesBlock]) = {
    val (droppedHistory, commonHistory) = history.splitOnCondReversed(_.ref.height > height)
    (WavesBranch(commonHistory, height), droppedHistory)
  }

  def dropAll: (WavesBranch, List[WavesBlock]) = (WavesBranch(Nil, history.lastOption.fold(height)(x => math.max(0, x.ref.height - 1))), history)

  private def mkHardenedBlock(blocks: NonEmptyList[WavesBlock]): WavesBlock = blocks.reduce(WavesBranch.blockSemigroup)
}

object WavesBranch {

  private[WavesBranch] val blockSemigroup = new Semigroup[WavesBlock] {

    override def combine(x: WavesBlock, y: WavesBlock): WavesBlock = {
      require(y.reference == x.ref.id, "y should reference to x")
      WavesBlock(
        ref = y.ref,
        reference = x.reference,
        changes = x.changes |+| y.changes,
        tpe = x.tpe
      )
    }

  }

  // TODO better name
  /**
   * @return (droppedBlocks, restHistory)
   */
  def dropPreviousFork(newKeyBlock: WavesBlock, history: List[WavesBlock]): Either[String, (List[WavesBlock], List[WavesBlock])] = {
    require(newKeyBlock.tpe == WavesBlock.Type.FullBlock, s"Expected $newKeyBlock to be a block, not a micro block!")
    dropPreviousFork(newKeyBlock, dropped = List.empty, restHistory = history)
  }

  @tailrec
  private def dropPreviousFork(
    newKeyBlock: WavesBlock,
    dropped: List[WavesBlock],
    restHistory: List[WavesBlock]
  ): Either[String, (List[WavesBlock], List[WavesBlock])] = restHistory match {
    case Nil => (dropped, restHistory).asRight
    case x :: tailRestHistory =>
      if (newKeyBlock.reference == x.ref.id) (dropped, restHistory).asRight
      else if (x.tpe == WavesBlock.Type.MicroBlock) dropPreviousFork(newKeyBlock, x :: dropped, tailRestHistory)
      else {
        val droppedRefs = dropped.map(_.ref).mkString(", ")
        s"The new block ${newKeyBlock.ref} must reference (${newKeyBlock.reference}) the one of the previous micro blocks ($droppedRefs) or the previous key block (${x.ref})".asLeft
      }
  }

}
