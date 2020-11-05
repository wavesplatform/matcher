package com.wavesplatform.dex.grpc.integration.clients.state

import cats.Semigroup
import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.ListOps.Ops
import com.wavesplatform.dex.grpc.integration.clients.state.BlockchainStatus.DiffIndex

/**
 * @param history Contains micro blocks
 */
case class WavesFork(history: List[BaseBlock]) {

  def withBlock(block: WavesBlock): Either[String, WavesFork] = history match {
    case Nil => WavesFork(block :: history).asRight
    case prev :: _ =>
      if (canAppend(prev, block)) {
        val (microBlocks, blocks) = history.splitOnCondReversed(_.tpe == BaseBlock.Type.MicroBlock)
        if (microBlocks.isEmpty) WavesFork(block :: blocks).asRight
        else blocks match {
          case Nil => throw new RuntimeException("Imposibru")
          case keyBlock :: restBlocks =>
            val liquidBlock = NonEmptyList(keyBlock, microBlocks)
            WavesFork(block :: mkHardenedBlock(liquidBlock) :: restBlocks).asRight
        }
      } else s"A new block ${block.ref} (reference=${block.reference}) must continue the chain with the previous one ${prev.ref}".asLeft
  }

  def withMicroBlock(microBlock: WavesMicroBlock): Either[String, WavesFork] = history match {
    case Nil => s"Can't attach a micro block $microBlock to empty chain".asLeft
    case prev :: _ =>
      if (canAppend(prev, microBlock)) WavesFork(microBlock :: history).asRight
      else
        s"A new micro block ${microBlock.ref} (reference=${microBlock.reference}) must continue the chain with the previous one ${prev.ref}".asLeft
  }

  def dropAfter(ref: BlockRef): (Option[WavesFork], DiffIndex) = {
    val (droppedHistory, commonHistory) = history.splitOnCondReversed(x => !(x.ref.height == ref.height && x.ref.id == ref.id))
    val fork = if (commonHistory.isEmpty) none else WavesFork(commonHistory).some
    (fork, droppedHistory.foldMap(_.diffIndex))
  }

  private def canAppend(prev: BaseBlock, newBlock: BaseBlock): Boolean =
    (prev.ref.height + 1) == newBlock.ref.height && prev.ref.id == newBlock.reference

  private def mkHardenedBlock(blocks: NonEmptyList[BaseBlock]): BaseBlock = blocks.reduce(WavesFork.blockSemigroup)
}

object WavesFork {

  private[WavesFork] val blockSemigroup = new Semigroup[BaseBlock] {

    override def combine(x: BaseBlock, y: BaseBlock): BaseBlock = BaseBlock(
      ref = y.ref,
      reference = y.reference,
      changes = x.changes |+| y.changes,
      tpe = x.tpe
    )

  }

}
