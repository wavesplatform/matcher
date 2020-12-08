package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.grpc.integration.clients.domain.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent.WavesNodeUtxEvent

import scala.collection.immutable.Queue

// TODO DEX-1004
case class StatusUpdate(
  newStatus: BlockchainStatus,
  updatedBalances: BlockchainBalance = Monoid.empty[BlockchainBalance],
  requestBalances: DiffIndex = Monoid.empty[DiffIndex],
  updatedLastBlockHeight: LastBlockHeight = LastBlockHeight.NotChanged, // DEX-999 Probably need to move to Observer
  processUtxEvents: Queue[WavesNodeUtxEvent] = Queue.empty,
  requestNextBlockchainEvent: Boolean = false // DEX-999 Can't be true if LastBlockHeight.RestartRequired
) {

  override def toString: String =
    s"StatusUpdate($newStatus, ub=$updatedBalances, rb=$requestBalances, lbh=$updatedLastBlockHeight, utx=${processUtxEvents.size}, rnbe=$requestNextBlockchainEvent)"

}

object StatusUpdate {

  sealed trait LastBlockHeight extends Product with Serializable

  object LastBlockHeight {
    case object NotChanged extends LastBlockHeight
    case class Updated(to: Int) extends LastBlockHeight
    case class RestartRequired(from: Int) extends LastBlockHeight
  }

}
