package com.wavesplatform.dex.grpc.integration.clients.state

import com.wavesplatform.dex.domain.account.Address

import scala.collection.immutable.Queue

sealed trait BlockchainState extends Product with Serializable {
  def blockInfo: BlockInfo
}

object BlockchainState {

  case class Normal(data: BlockchainData, liquidBlocks: List[BlockchainData]) extends BlockchainState {
    override def blockInfo: BlockInfo = data.blockInfo
  }

  case class TransientRollback(commonBlockInfo: BlockInfo, orig: Normal, accumulated: BlockchainData) extends BlockchainState {
    override def blockInfo: BlockInfo = commonBlockInfo
  }

  // case class TransientRollbackMicro() extends BlockchainState {}

  case class TransientResolving(orig: BlockchainData, waitInfoFor: Set[Address], stash: Queue[BlockchainEvent]) extends BlockchainState {
    override def blockInfo: BlockInfo = orig.blockInfo
  }

}
