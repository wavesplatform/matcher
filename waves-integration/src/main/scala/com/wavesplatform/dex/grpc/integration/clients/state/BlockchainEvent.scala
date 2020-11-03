package com.wavesplatform.dex.grpc.integration.clients.state

sealed trait BlockchainEvent extends Product with Serializable

object BlockchainEvent {
  case class AppendBlock(block: WavesBlock) extends BlockchainEvent
  case class AppendMicroBlock(microBlock: WavesMicroBlock) extends BlockchainEvent
  case class RollbackTo(commonBlockRef: BlockRef) extends BlockchainEvent
  // Could happen on appending of a key block
  case class RollbackMicroBlocks(commonBlockRef: BlockRef) extends BlockchainEvent
  case class BalanceUpdates(updates: BlockchainBalance) extends BlockchainEvent
}
