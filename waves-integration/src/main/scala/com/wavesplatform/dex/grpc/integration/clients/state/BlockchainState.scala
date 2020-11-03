package com.wavesplatform.dex.grpc.integration.clients.state

import cats.data.NonEmptyList
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.account.Address

import scala.collection.immutable.Queue

sealed trait BlockchainState extends Product with Serializable

object BlockchainState {

  case class Normal(history: WavesFork, liquidBlocks: NonEmptyList[WavesBlock]) extends BlockchainState {
    def latestLiquidBlock: WavesBlock = liquidBlocks.head

    def withBlock(block: WavesBlock): Normal = Normal(
      history = history.withBlock(latestLiquidBlock),
      liquidBlocks = NonEmptyList.one(block) // Reset liquid blocks, because we got a key block
    )

    def withMicroBlock(microBlock: WavesMicroBlock): Normal = copy(
      liquidBlocks = WavesBlock(
        BaseBlock(
          blockInfo = microBlock.blockInfo,
          changes = latestLiquidBlock.changes |+| microBlock.changes
        ) :: liquidBlocks
      )
    )

  }

  case class TransientRollback(commonBlockInfo: BlockRef, orig: Normal, fork: WavesFork) extends BlockchainState

  case class TransientRollbackMicro(commonBlockInfo: BlockRef, orig: Normal, liquidBlocks: List[WavesFork]) extends BlockchainState

  case class TransientResolving(orig: WavesFork, waitInfoFor: Set[Address], stash: Queue[BlockchainEvent]) extends BlockchainState

}
