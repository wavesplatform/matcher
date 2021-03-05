package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight

case class StatusUpdate(
  newStatus: BlockchainStatus,
  updatedBalances: BlockchainBalance = Monoid.empty[BlockchainBalance],
  requestBalances: DiffIndex = Monoid.empty[DiffIndex],
  updatedLastBlockHeight: LastBlockHeight = LastBlockHeight.NotChanged, // DEX-999 Probably need to move to Observer
  utxUpdate: UtxUpdate = Monoid.empty[UtxUpdate],
  requestNextBlockchainEvent: Boolean = false // DEX-999 Can't be true if LastBlockHeight.RestartRequired
) {

  override def toString: String =
    s"StatusUpdate($newStatus, ub=$updatedBalances, rb=$requestBalances, lbh=$updatedLastBlockHeight, rnbe=$requestNextBlockchainEvent, $utxUpdate)"

}

object StatusUpdate {

  sealed trait LastBlockHeight extends Product with Serializable

  object LastBlockHeight {
    case object NotChanged extends LastBlockHeight

    // We confirm the latest event from the blockchain updates stream
    case class Updated(to: Int) extends LastBlockHeight

    // Something is broken, so we need to rollback and reconnect
    case object RestartRequired extends LastBlockHeight
  }

}
