package com.wavesplatform.dex.grpc.integration.clients.state

sealed trait BlockchainEvent extends Product with Serializable

object BlockchainEvent {

  case class Appended(block: WavesBlock) extends BlockchainEvent {
    override def toString: String = s"Appended(${block.tpe}, h=${block.ref.height}, ${block.ref.id})"
  }

  // Could also happen on appending of a key block
  case class RolledBackTo(commonBlockRef: BlockRef) extends BlockchainEvent {
    override def toString: String = s"RolledBackTo(${commonBlockRef.height}, ${commonBlockRef.id})"
  }

  case class DataReceived(updates: BlockchainBalance) extends BlockchainEvent {
    override def toString: String = "DataReceived"
  }

}
