package com.wavesplatform.dex.grpc.integration.clients.state

import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import com.wavesplatform.dex.grpc.integration.clients.state.StatusUpdate.LastBlockHeight

// TODO replace with interface with methods?
case class StatusUpdate(
  newStatus: BlockchainStatus,
  updatedBalances: BlockchainBalance = Monoid.empty[BlockchainBalance],
  requestBalances: DiffIndex = Monoid.empty[DiffIndex],
  updatedLastBlockHeight: LastBlockHeight = LastBlockHeight.NotChanged
) {
  override def toString: String = s"StatusUpdate($newStatus, ub=$updatedBalances, rb=$requestBalances, lbh=$updatedLastBlockHeight)"
}

object StatusUpdate {

  implicit val statusUpdateSemigroup: Semigroup[StatusUpdate] = { (x: StatusUpdate, y: StatusUpdate) =>
    StatusUpdate(
      newStatus = y.newStatus,
      updatedBalances = x.updatedBalances |+| y.updatedBalances,
      requestBalances = x.requestBalances |+| y.requestBalances,
      updatedLastBlockHeight = y.updatedLastBlockHeight |+| x.updatedLastBlockHeight
    )
  }

  sealed trait LastBlockHeight extends Product with Serializable

  object LastBlockHeight {
    case object NotChanged extends LastBlockHeight
    case class Updated(to: Int) extends LastBlockHeight
    case class RestartRequired(at: Int) extends LastBlockHeight // TODO Make restart in one place

    implicit val heightUpdateSemigroup: Semigroup[LastBlockHeight] = {
      case (NotChanged, y) => y
      case (x, NotChanged) => x
      case (_, y: RestartRequired) => y
      case (x: RestartRequired, _) => x
      case (x: Updated, y: Updated) => if (x.to > y.to) x else y
    }

  }

}
