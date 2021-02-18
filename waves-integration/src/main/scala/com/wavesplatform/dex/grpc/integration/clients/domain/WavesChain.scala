package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.vector._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collections.VectorOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesChain.dropLiquidBlock

import scala.annotation.tailrec

/**
 * @param history Contains micro blocks
 * @param blocksCapacity How many full blocks we can append
 */
case class WavesChain(history: Vector[WavesBlock], height: Int, blocksCapacity: Int) {
  {
    val historyHeight = history.headOption.map(_.ref.height)
    require(historyHeight.forall(_ == height), s"height=$height should correspond the height of the last block=$historyHeight")
    require(blocksCapacity >= 0, "blocksCapacity >= 0")
  }

  // We prepend block, because it is easier to use dropWhile

  def isEmpty: Boolean = history.isEmpty

  def last: Option[WavesBlock] = history.headOption

  def has(ref: BlockRef): Boolean = history.exists(_.ref == ref)

  def withBlock(block: WavesBlock): Either[String, WavesChain] =
    if (block.tpe == WavesBlock.Type.FullBlock) withFullBlock(block)
    else withMicroBlock(block)

  /**
   * It is expected, that block references the last block in the history
   * @return Guarantees WavesFork is not empty
   */
  private def withFullBlock(block: WavesBlock): Either[String, WavesChain] =
    if (block.ref.height == height + 1)
      history.headOption match {
        case None => WavesChain(history.prepended(block), block.ref.height, blocksCapacity = blocksCapacity - 1).asRight
        case Some(prev) =>
          if (block.reference == prev.ref.id) {
            val (liquidBlock, restHistory) = dropLiquidBlock(block, history)
            val newHistory = liquidBlock match {
              case Nil => restHistory.prepended(block)
              case x :: xs => restHistory.prepended(mkHardenedBlock(NonEmptyList(x, xs))).prepended(block)
            }
            if (blocksCapacity == 0) WavesChain(newHistory.dropRight(1), block.ref.height, blocksCapacity = 0).asRight
            else WavesChain(newHistory, block.ref.height, blocksCapacity = blocksCapacity - 1).asRight
          } else s"The new block ${block.ref} (reference=${block.reference.take(5)}) must be after ${prev.ref}".asLeft
      }
    else s"The new block ${block.ref} (reference=${block.reference.take(5)}) must be on height ${height + 1}".asLeft

  private def withMicroBlock(microBlock: WavesBlock): Either[String, WavesChain] = history.headOption match {
    case None => s"Can't attach a micro block $microBlock to empty chain".asLeft
    case Some(prev) =>
      if (microBlock.ref.height == prev.ref.height && microBlock.reference == prev.ref.id)
        WavesChain(history.prepended(microBlock), microBlock.ref.height, blocksCapacity = blocksCapacity).asRight
      else
        s"The new micro block ${microBlock.ref} (reference=${microBlock.reference.take(5)}) must reference the last block ${prev.ref}".asLeft
  }

  def diffIndex: DiffIndex = history.foldMap(_.diffIndex)

  def withoutLast: (WavesChain, Option[WavesBlock]) =
    if (isEmpty) (this, None)
    else {
      val droppedBlock = history.headOption
      val heightCorrection = droppedBlock.filter(_.tpe == WavesBlock.Type.FullBlock).fold(0)(_ => 1)
      (WavesChain(history.tail, height - heightCorrection, blocksCapacity = blocksCapacity + heightCorrection), droppedBlock)
    }

  /**
   * @return (new chain, dropped blocks), where dropped blocks are ordered from oldest to newest
   */
  def dropAfter(ref: BlockRef): (WavesChain, List[WavesBlock]) =
    // TODO DEX-1032
    if (ref.height > height) (this, List.empty)
    else {
      val (droppedBlocks, commonHistory) = history.splitOnCondReversed(x => !(x.ref.height == ref.height && x.ref.id == ref.id))
      val droppedFullBlocksNumber = droppedBlocks.count(_.tpe == WavesBlock.Type.FullBlock)
      (WavesChain(commonHistory, ref.height, blocksCapacity = blocksCapacity + droppedFullBlocksNumber), droppedBlocks)
    }

  /**
   * @return (new chain, dropped blocks), where dropped blocks are ordered from oldest to newest
   */
  def dropAfter(h: Int): (WavesChain, List[WavesBlock]) =
    // TODO DEX-1032
    if (h > height) (this, List.empty)
    else {
      val (droppedBlocks, commonHistory) = history.splitOnCondReversed(_.ref.height > h)
      val droppedFullBlocksNumber = droppedBlocks.count(_.tpe == WavesBlock.Type.FullBlock)
      (WavesChain(commonHistory, h, blocksCapacity = blocksCapacity + droppedFullBlocksNumber), droppedBlocks)
    }

  private def mkHardenedBlock(blocks: NonEmptyList[WavesBlock]): WavesBlock = blocks.reduce(WavesChain.blockSemigroup)

  override def toString: String = s"WavesChain(his=${history.map(x => s"${x.tpe}-${x.ref}")}, h=$height)"
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
        tpe = x.tpe,
        confirmedTxs = x.confirmedTxs ++ y.confirmedTxs
      )
    }

  }

  /**
   * @return (liquidBlock, restHistory)
   */
  def dropLiquidBlock(newFullBlock: WavesBlock, history: Vector[WavesBlock]): (List[WavesBlock], Vector[WavesBlock]) =
    dropLiquidBlock(newFullBlock, liquidBlock = List.empty, restHistory = history)

  @tailrec
  private def dropLiquidBlock(
    newFullBlock: WavesBlock,
    liquidBlock: List[WavesBlock],
    restHistory: Vector[WavesBlock]
  ): (List[WavesBlock], Vector[WavesBlock]) = restHistory.headOption match {
    case None => (liquidBlock, restHistory)
    case Some(x) =>
      if (x.tpe == WavesBlock.Type.MicroBlock) dropLiquidBlock(newFullBlock, x :: liquidBlock, restHistory.tail)
      else if (liquidBlock.isEmpty) (liquidBlock, restHistory) // No liquid block, x is a full block
      else (x :: liquidBlock, restHistory.tail) // We have a liquid block, x is a key block
  }

  /**
   * @return (dropped blocks of chain1, dropped blocks of chain2), where blocks ordered in natural order, e.g.:
   *         full block h=1, full block h=2, micro block 1, micro block 2
   */
  def dropDifference(chain1: WavesChain, chain2: WavesChain): (List[WavesBlock], List[WavesBlock]) =
    dropDifference(
      chain1 = chain1,
      acc1 = List.empty,
      chain1RestMicroBlockNumber = chain1.history.view.takeWhile(_.tpe == WavesBlock.Type.MicroBlock).size,
      chain2 = chain2,
      acc2 = List.empty,
      chain2RestMicroBlockNumber = chain2.history.view.takeWhile(_.tpe == WavesBlock.Type.MicroBlock).size
    )

  @tailrec
  private def dropDifference(
    chain1: WavesChain,
    acc1: List[WavesBlock],
    chain1RestMicroBlockNumber: Int,
    chain2: WavesChain,
    acc2: List[WavesBlock],
    chain2RestMicroBlockNumber: Int
  ): (List[WavesBlock], List[WavesBlock]) =
    (chain1.last, chain2.last) match {
      case (Some(block1), Some(block2)) =>
        if (block1.ref == block2.ref) (acc1, acc2)
        else if (chain1RestMicroBlockNumber > chain2RestMicroBlockNumber) {
          val (updatedChain1, droppedBlock1) = chain1.withoutLast
          dropDifference(
            updatedChain1,
            droppedBlock1.fold(acc1)(_ :: acc1),
            chain1RestMicroBlockNumber - 1,
            chain2,
            acc2,
            chain2RestMicroBlockNumber
          )
        } else if (chain1RestMicroBlockNumber < chain2RestMicroBlockNumber) {
          val (updatedChain2, droppedBlock2) = chain2.withoutLast
          dropDifference(
            chain1,
            acc1,
            chain1RestMicroBlockNumber,
            updatedChain2,
            droppedBlock2.fold(acc2)(_ :: acc2),
            chain2RestMicroBlockNumber - 1
          )
        } else if (block1.ref.height > block2.ref.height) {
          val (updatedChain1, droppedBlock1) = chain1.withoutLast
          dropDifference(
            updatedChain1,
            droppedBlock1.fold(acc1)(_ :: acc1),
            chain1RestMicroBlockNumber - droppedBlock1.filter(_.tpe == WavesBlock.Type.MicroBlock).fold(0)(_ => 1),
            chain2,
            acc2,
            chain2RestMicroBlockNumber
          )
        } else if (block1.ref.height < block2.ref.height) {
          val (updatedChain2, droppedBlock2) = chain2.withoutLast
          dropDifference(
            chain1,
            acc1,
            chain1RestMicroBlockNumber,
            updatedChain2,
            droppedBlock2.fold(acc2)(_ :: acc2),
            chain2RestMicroBlockNumber - droppedBlock2.filter(_.tpe == WavesBlock.Type.MicroBlock).fold(0)(_ => 1)
          )
        } else {
          val (updatedChain1, droppedBlock1) = chain1.withoutLast
          val (updatedChain2, droppedBlock2) = chain2.withoutLast
          dropDifference(
            updatedChain1,
            droppedBlock1.fold(acc1)(_ :: acc1),
            chain1RestMicroBlockNumber - droppedBlock1.filter(_.tpe == WavesBlock.Type.MicroBlock).fold(0)(_ => 1),
            updatedChain2,
            droppedBlock2.fold(acc2)(_ :: acc2),
            chain2RestMicroBlockNumber - droppedBlock2.filter(_.tpe == WavesBlock.Type.MicroBlock).fold(0)(_ => 1)
          )
        }
      case (Some(_), None) => (chain1.history.reverse.toList ::: acc1, acc2)
      case (None, Some(_)) => (acc1, chain2.history.reverse.toList ::: acc2)
      case _ => (acc1, acc2)
    }

}
