package com.wavesplatform.dex.grpc.integration.clients.domain

import com.wavesplatform.dex.meta.getSimpleName

sealed trait BlockchainStatus extends Product with Serializable {
  def name: String = getSimpleName(this)
}

object BlockchainStatus {

  case class Normal(main: WavesChain) extends BlockchainStatus {
    override def toString: String = s"Normal(${main.history.headOption.map(_.ref)})"
  }

  case class TransientRollback(fork: WavesFork, utxUpdate: UtxUpdate) extends BlockchainStatus {
    override def toString: String = s"TransientRollback(f=$fork, $utxUpdate)"
  }

  case class TransientResolving(main: WavesChain, stashChanges: BlockchainBalance, utxUpdate: UtxUpdate) extends BlockchainStatus {
    override def toString: String = s"TransientResolving(${main.history.headOption.map(_.ref)}, $utxUpdate)"
  }

}
