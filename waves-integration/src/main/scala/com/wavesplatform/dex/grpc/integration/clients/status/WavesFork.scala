package com.wavesplatform.dex.grpc.integration.clients.status

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.status.WavesFork.dropPreviousFork

import scala.annotation.tailrec

/**
 * @param history Contains micro blocks
 */
case class WavesFork(history: List[WavesBlock]) { // TODO cut to last N blocks is required!

  def withBlock(block: WavesBlock): Either[String, WavesFork] =
    if (block.tpe == WavesBlock.Type.Block) withFullBlock(block)
    else withMicroBlock(block)

  /**
   * @return Guarantees WavesFork is not empty
   */
  private def withFullBlock(block: WavesBlock): Either[String, WavesFork] = history match {
    case Nil => WavesFork(block :: history).asRight
    case prev :: _ =>
      if (block.ref.height == prev.ref.height + 1)
        dropPreviousFork(block, history).map {
          case (droppedBlocks, historyAfterCut) =>
            val (microBlocks, blocksHistory) = historyAfterCut.splitOnCondReversed(_.tpe == WavesBlock.Type.MicroBlock)
            if (microBlocks.isEmpty) WavesFork(block :: blocksHistory)
            else blocksHistory match {
              case Nil => throw new RuntimeException("Imposibru")
              case keyBlock :: restBlocks =>
                val liquidBlock = NonEmptyList(keyBlock, microBlocks) // Have the direct order
                WavesFork(block :: mkHardenedBlock(liquidBlock) :: restBlocks)
            }
        }
      else s"The new block ${block.ref} (reference=${block.reference}) must be after ${prev.ref}".asLeft
  }

  private def withMicroBlock(microBlock: WavesBlock): Either[String, WavesFork] = history match {
    case Nil => s"Can't attach a micro block $microBlock to empty chain".asLeft
    case prev :: _ =>
      if (microBlock.ref.height == prev.ref.height && microBlock.reference == prev.ref.id) WavesFork(microBlock :: history).asRight
      else
        s"The new micro block ${microBlock.ref} (reference=${microBlock.reference}) must reference the last block ${prev.ref}".asLeft
  }

  def diffIndex: DiffIndex = history.foldMap(_.diffIndex)

  // TODO replace by dropPreviousFork
  def dropAfter(ref: BlockRef): (WavesFork, DiffIndex) = {
    val (droppedHistory, commonHistory) = history.splitOnCondReversed(x => !(x.ref.height == ref.height && x.ref.id == ref.id))
    (WavesFork(commonHistory), droppedHistory.foldMap(_.diffIndex))
  }

  def dropAfter(height: Int): (WavesFork, DiffIndex) = {
    val (droppedHistory, commonHistory) = history.splitOnCondReversed(_.ref.height > height)
    (WavesFork(commonHistory), droppedHistory.foldMap(_.diffIndex))
  }

  def dropAll: (WavesFork, DiffIndex) = (WavesFork(Nil), history.foldMap(_.diffIndex))

  private def mkHardenedBlock(blocks: NonEmptyList[WavesBlock]): WavesBlock = blocks.reduce(WavesFork.blockSemigroup)
}

object WavesFork {

  private[WavesFork] val blockSemigroup = new Semigroup[WavesBlock] {

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

  /**
   * @return (droppedBlocks, restHistory)
   */
  def dropPreviousFork(newKeyBlock: WavesBlock, history: List[WavesBlock]): Either[String, (List[WavesBlock], List[WavesBlock])] = {
    require(newKeyBlock.tpe == WavesBlock.Type.Block, s"Expected $newKeyBlock to be a block, not a micro block!")
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
