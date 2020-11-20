package com.wavesplatform.dex.grpc.integration.clients.status

import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import com.wavesplatform.dex.grpc.integration.clients.status.StatusUpdate.LastBlockHeight
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent.WavesNodeUtxEvent

import scala.collection.immutable.Queue

// TODO replace with interface with methods?
case class StatusUpdate(
  newStatus: BlockchainStatus,
  updatedBalances: BlockchainBalance = Monoid.empty[BlockchainBalance],
  requestBalances: DiffIndex = Monoid.empty[DiffIndex],
  updatedLastBlockHeight: LastBlockHeight = LastBlockHeight.NotChanged,
  processUtxEvents: Queue[WavesNodeUtxEvent] = Queue.empty
) {
  override def toString: String = s"StatusUpdate($newStatus, ub=$updatedBalances, rb=$requestBalances, lbh=$updatedLastBlockHeight, utx=${processUtxEvents.size})"
}

object StatusUpdate {

  implicit val statusUpdateSemigroup: Semigroup[StatusUpdate] = { (x: StatusUpdate, y: StatusUpdate) =>
    StatusUpdate(
      newStatus = y.newStatus,
      updatedBalances = x.updatedBalances |+| y.updatedBalances,
      requestBalances = x.requestBalances |+| y.requestBalances,
      updatedLastBlockHeight = y.updatedLastBlockHeight |+| x.updatedLastBlockHeight,
      processUtxEvents = x.processUtxEvents.enqueueAll(y.processUtxEvents)
    )
  }

  sealed trait LastBlockHeight extends Product with Serializable

  object LastBlockHeight {
    case object NotChanged extends LastBlockHeight
    case class Updated(to: Int) extends LastBlockHeight
    case class RestartRequired(from: Int) extends LastBlockHeight

    implicit val heightUpdateSemigroup: Semigroup[LastBlockHeight] = {
      case (NotChanged, y) => y
      case (x, NotChanged) => x
      case (_, y: RestartRequired) => y
      case (x: RestartRequired, _) => x
      case (x: Updated, y: Updated) => if (x.to > y.to) x else y
    }

  }

}
