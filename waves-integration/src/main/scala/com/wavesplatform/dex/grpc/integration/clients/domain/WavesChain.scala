package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.queue._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.QueueOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesChain.dropLiquidBlock

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * TODO DEX-1008 A constructor with auto height when passed non empty list
 * TODO DEX-1008 Cut to last N blocks is required to reduce amount of stored blocks!
 *
 * @param history Contains micro blocks
 * @param blocksCapacity How many full blocks we can append
 */
case class WavesChain(history: Queue[WavesBlock], height: Int, blocksCapacity: Int) {
  require(history.headOption.map(_.ref.height).forall(_ == height), "height corresponds last block")
  require(blocksCapacity >= 0, "blocksCapacity >= 0")

  // We prepend block, because it is easier to use dropWhile

  def isEmpty: Boolean = history.isEmpty

  def last: Option[WavesBlock] = history.headOption

  // TODO dropping!
  def withBlock(block: WavesBlock): Either[String, WavesChain] =
    if (block.tpe == WavesBlock.Type.FullBlock) withFullBlock(block)
    else withMicroBlock(block)

  /**
   * It is expected, that block references the last block in the history
   * @return Guarantees WavesFork is not empty
   */
  private def withFullBlock(block: WavesBlock): Either[String, WavesChain] = history.headOption match {
    case None => WavesChain(history.prepended(block), block.ref.height, blocksCapacity = blocksCapacity - 1).asRight
    case Some(prev) =>
      if (block.ref.height == prev.ref.height + 1 && block.reference == prev.ref.id) {
        val (liquidBlock, restHistory) = dropLiquidBlock(block, history)
        val newHistory = liquidBlock match {
          case Nil => restHistory.prepended(block)
          case x :: xs => restHistory.prepended(mkHardenedBlock(NonEmptyList(x, xs))).prepended(block)
        }
        WavesChain(newHistory, block.ref.height, blocksCapacity = blocksCapacity - 1).asRight
      } else s"The new block ${block.ref} (reference=${block.reference}) must be after ${prev.ref}".asLeft
  }

  private def withMicroBlock(microBlock: WavesBlock): Either[String, WavesChain] = history.headOption match {
    case None => s"Can't attach a micro block $microBlock to empty chain".asLeft
    case Some(prev) =>
      if (microBlock.ref.height == prev.ref.height && microBlock.reference == prev.ref.id)
        WavesChain(history.prepended(microBlock), microBlock.ref.height, blocksCapacity = blocksCapacity).asRight
      else
        s"The new micro block ${microBlock.ref} (reference=${microBlock.reference}) must reference the last block ${prev.ref}".asLeft
  }

  def diffIndex: DiffIndex = history.foldMap(_.diffIndex)

  // TODO tests
  def withoutLastLiquidOrFull: WavesChain = {
    val heightCorrection = if (history.isEmpty) 0 else 1
    val updatedHistory =
      if (history.isEmpty) history
      else if (history.headOption.exists(_.tpe == WavesBlock.Type.MicroBlock))
        // Remove a liquid block. tail is safe, because we can't append a micro block without a block in the history
        history.dropWhile(_.tpe == WavesBlock.Type.MicroBlock).tail
      else history.tail // Remove a full block
    WavesChain(updatedHistory, height - heightCorrection, blocksCapacity = blocksCapacity + heightCorrection)
  }

  // TODO tests
  def withoutLast: (WavesChain, Option[WavesBlock]) =
    if (isEmpty) (this, None)
    else {
      val droppedBlock = history.headOption
      val heightCorrection = droppedBlock.filter(_.tpe == WavesBlock.Type.FullBlock).fold(0)(_ => 1)
      (WavesChain(history.tail, height - heightCorrection, blocksCapacity = blocksCapacity + heightCorrection), droppedBlock)
    }

  // TODO tests
  /**
   * @return (new chain, dropped blocks), where dropped blocks are ordered from oldest to newest
   */
  def dropAfter(ref: BlockRef): (WavesChain, List[WavesBlock]) = {
    // TODO DEX-1032
    val (droppedBlocks, commonHistory) = history.splitOnCondReversed(x => !(x.ref.height == ref.height && x.ref.id == ref.id))
    val droppedFullBlocksNumber = droppedBlocks.count(_.tpe == WavesBlock.Type.FullBlock)
    (WavesChain(commonHistory, ref.height, blocksCapacity = blocksCapacity + droppedFullBlocksNumber), droppedBlocks)
  }

  def dropAfter(height: Int): (WavesChain, List[WavesBlock]) = {
    // TODO DEX-1032
    val (droppedBlocks, commonHistory) = history.splitOnCondReversed(_.ref.height > height)
    val droppedFullBlocksNumber = droppedBlocks.count(_.tpe == WavesBlock.Type.FullBlock)
    (WavesChain(commonHistory, height, blocksCapacity = blocksCapacity + droppedFullBlocksNumber), droppedBlocks)
  }

  private def mkHardenedBlock(blocks: NonEmptyList[WavesBlock]): WavesBlock = blocks.reduce(WavesChain.blockSemigroup)

  override def toString: String = s"WavesChain(his=${history.map(_.ref)}, h=$height)"
}

object WavesChain {

  // DEX-1002
  private[WavesChain] val blockSemigroup = new Semigroup[WavesBlock] {

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
  def dropLiquidBlock(newFullBlock: WavesBlock, history: Queue[WavesBlock]): (List[WavesBlock], Queue[WavesBlock]) =
    dropLiquidBlock(newFullBlock, liquidBlock = List.empty, restHistory = history)

  @tailrec
  private def dropLiquidBlock(
    newFullBlock: WavesBlock,
    liquidBlock: List[WavesBlock],
    restHistory: Queue[WavesBlock]
  ): (List[WavesBlock], Queue[WavesBlock]) = restHistory.headOption match {
    case None => (liquidBlock, restHistory)
    case Some(x) =>
      if (x.tpe == WavesBlock.Type.MicroBlock) dropLiquidBlock(newFullBlock, x :: liquidBlock, restHistory.tail)
      // TODO isEmpty
      else if (liquidBlock.nonEmpty) (x :: liquidBlock, restHistory.tail) // We have a liquid block, x is a key block
      else (liquidBlock, restHistory) // No liquid block, x is a full block
  }

  /**
   * @return (dropped blocks of chain1, dropped blocks of chain2), where blocks ordered in natural order, e.g.:
   *         full block h=1, full block h=2, micro block 1, micro block 2
   */
  def dropDifference(chain1: WavesChain, chain2: WavesChain): (List[WavesBlock], List[WavesBlock]) =
    dropDifference(chain1, List.empty, chain2, List.empty)

  @tailrec
  private def dropDifference(
    chain1: WavesChain,
    acc1: List[WavesBlock],
    chain2: WavesChain,
    acc2: List[WavesBlock]
  ): (List[WavesBlock], List[WavesBlock]) =
    (chain1.last, chain2.last) match {
      case (Some(block1), Some(block2)) =>
        if (block1.ref == block2.ref) (acc1, acc2)
        else if (block1.ref.height > block2.ref.height) {
          val (updatedChain1, droppedBlock1) = chain1.withoutLast
          dropDifference(updatedChain1, droppedBlock1.fold(acc1)(_ :: acc1), chain2, acc2)
        } else if (block1.ref.height < block2.ref.height) {
          val (updatedChain2, droppedBlock2) = chain2.withoutLast
          dropDifference(chain1, acc1, updatedChain2, droppedBlock2.fold(acc2)(_ :: acc2))
        } else {
          val (updatedChain1, droppedBlock1) = chain1.withoutLast
          val (updatedChain2, droppedBlock2) = chain2.withoutLast
          dropDifference(updatedChain1, droppedBlock1.fold(acc1)(_ :: acc1), updatedChain2, droppedBlock2.fold(acc2)(_ :: acc2))
        }
      case _ => (acc1, acc2)
    }

}
