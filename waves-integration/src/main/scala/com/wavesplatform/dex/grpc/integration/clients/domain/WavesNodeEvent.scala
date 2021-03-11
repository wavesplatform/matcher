package com.wavesplatform.dex.grpc.integration.clients.domain

import com.google.protobuf.ByteString
import com.wavesplatform.dex.grpc.integration.services.UtxTransaction
import scorex.util.encode.Base58

sealed trait WavesNodeEvent extends Product with Serializable

object WavesNodeEvent {

  sealed trait BlockchainUpdates

  case class Appended(block: WavesBlock) extends WavesNodeEvent with BlockchainUpdates {
    override def toString: String = s"Appended(${block.tpe}, h=${block.ref.height}, ${block.ref.id})"
  }

  // Could also happen on appending of a key block
  // It is expected, that we get Appended after RolledBack
  case class RolledBack(to: RolledBack.To) extends WavesNodeEvent with BlockchainUpdates

  object RolledBack {
    sealed trait To extends Product with Serializable

    object To {

      // Only from the blockchain updates stream.
      // Either rollback to a block, or to a micro block (micro fork).
      case class CommonBlockRef(ref: BlockRef) extends To {
        override def toString: String = s"CommonBlockRef(${ref.height}, ${ref.id})"
      }

      // Only during issues: a wrong block, disconnects
      case class Height(h: Int) extends To

    }

  }

  case class DataReceived(updates: BlockchainBalance) extends WavesNodeEvent {
    override def toString: String = "DataReceived"
  }

  case class UtxUpdated(addedTxs: Seq[UtxTransaction], failedTxs: Seq[UtxTransaction]) extends WavesNodeEvent {
    override def toString: String = s"UtxUpdated(a=${txsToString(addedTxs)}, f=${txsToString(failedTxs)})"
  }

  case class UtxSwitched(newTxs: Seq[UtxTransaction]) extends WavesNodeEvent {
    override def toString: String = s"UtxSwitched(${txsToString(newTxs)})"
  }

  sealed trait WavesNodeUtxEvent extends Product with Serializable

  object WavesNodeUtxEvent {
    case class Updated(newTxs: Seq[UtxTransaction], failedTxs: Seq[UtxTransaction]) extends WavesNodeUtxEvent
    case class Confirmed(txIds: Set[ByteString]) extends WavesNodeUtxEvent
    case class Switched(newTxs: Seq[UtxTransaction]) extends WavesNodeUtxEvent
  }

  // Utility

  implicit final class ByteStringOps(val self: ByteString) extends AnyVal {
    def toBase58: String = Base58.encode(self.toByteArray)
  }

  private def txsToString(txs: Seq[UtxTransaction]): String = txs.map(_.id.toBase58).mkString(", ")

}
