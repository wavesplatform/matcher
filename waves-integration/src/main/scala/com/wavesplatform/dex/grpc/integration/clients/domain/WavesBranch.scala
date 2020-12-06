package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesBranch.dropLiquidBlock

import scala.annotation.tailrec

/**
 * TODO DEX-1008 WavesChain? or history -> chain?
 * TODO DEX-1008 A constructor with auto height when passed non empty list
 * TODO DEX-1008 Cut to last N blocks is required to reduce amount of stored blocks!
 *
 * @param history Contains micro blocks
 */
case class WavesBranch(history: List[WavesBlock], height: Int) {

  require(history.headOption.map(_.ref.height).forall(_ == height), "height corresponds last block")

  def isEmpty: Boolean = history.isEmpty

  def withBlock(block: WavesBlock): Either[String, WavesBranch] =
    if (block.tpe == WavesBlock.Type.FullBlock) withFullBlock(block)
    else withMicroBlock(block)

  /**
   * It is expected, that block references the last block in the history
   * @return Guarantees WavesFork is not empty
   */
  private def withFullBlock(block: WavesBlock): Either[String, WavesBranch] = history match {
    case Nil => WavesBranch(block :: history, block.ref.height).asRight
    case prev :: _ =>
      if (block.ref.height == prev.ref.height + 1 && block.reference == prev.ref.id) {
        val (liquidBlock, restHistory) = dropLiquidBlock(block, history)
        val newHistory = liquidBlock match {
          case Nil => block :: restHistory
          case x :: xs => block :: mkHardenedBlock(NonEmptyList(x, xs)) :: restHistory
        }
        WavesBranch(newHistory, block.ref.height).asRight
      } else s"The new block ${block.ref} (reference=${block.reference}) must be after ${prev.ref}".asLeft
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

  def withoutLast: WavesBranch = {
    val updatedHistory =
      if (history.isEmpty) Nil
      else if (history.headOption.exists(_.tpe == WavesBlock.Type.MicroBlock))
        // Remove a liquid block. tail is safe, because we can't append a micro block without a block in the history
        history.dropWhile(_.tpe == WavesBlock.Type.MicroBlock).tail
      else history.tail // Remove a full block
    WavesBranch(updatedHistory, math.max(0, height - 1))
  }

  /**
   * @return (new branch, dropped blocks), where dropped blocks are ordered from oldest to newest
   */
  def dropAfter(ref: BlockRef): (WavesBranch, List[WavesBlock]) = {
    val (droppedBlocks, commonHistory) = history.splitOnCondReversed(x => !(x.ref.height == ref.height && x.ref.id == ref.id))
    (WavesBranch(commonHistory, ref.height), droppedBlocks)
  }

  def dropAfter(height: Int): (WavesBranch, List[WavesBlock]) = {
    val (droppedBlocks, commonHistory) = history.splitOnCondReversed(_.ref.height > height)
    (WavesBranch(commonHistory, height), droppedBlocks)
  }

  def dropAll: (WavesBranch, List[WavesBlock]) = (WavesBranch(Nil, history.lastOption.fold(height)(x => math.max(0, x.ref.height - 1))), history)

  private def mkHardenedBlock(blocks: NonEmptyList[WavesBlock]): WavesBlock = blocks.reduce(WavesBranch.blockSemigroup)

  override def toString: String = s"WavesBranch(his=${history.map(_.ref)}, h=$height)"
}

object WavesBranch {

  // DEX-1002
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

  /**
   * @return (liquidBlock, restHistory)
   */
  def dropLiquidBlock(newFullBlock: WavesBlock, history: List[WavesBlock]): (List[WavesBlock], List[WavesBlock]) =
    dropLiquidBlock(newFullBlock, liquidBlock = List.empty, restHistory = history)

  @tailrec
  private def dropLiquidBlock(
    newFullBlock: WavesBlock,
    liquidBlock: List[WavesBlock],
    restHistory: List[WavesBlock]
  ): (List[WavesBlock], List[WavesBlock]) = restHistory match {
    case Nil => (liquidBlock, restHistory)
    case x :: tailRestHistory =>
      if (x.tpe == WavesBlock.Type.MicroBlock) dropLiquidBlock(newFullBlock, x :: liquidBlock, tailRestHistory)
      else if (liquidBlock.nonEmpty) (x :: liquidBlock, tailRestHistory) // We have a liquid block, x is a key block
      else (liquidBlock, restHistory) // No liquid block, x is a full block
  }

}
