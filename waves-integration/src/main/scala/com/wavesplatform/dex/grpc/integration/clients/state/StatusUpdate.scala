package com.wavesplatform.dex.grpc.integration.clients.state

import cats.syntax.semigroup._
import cats.{Monoid, Semigroup}
import com.wavesplatform.dex.grpc.integration.clients.state.StatusUpdate.HeightUpdate

// TODO replace with interface with methods?
case class StatusUpdate(
  newStatus: BlockchainStatus,
  updatedBalances: BlockchainBalance = Monoid.empty,
  requestBalances: DiffIndex = Monoid.empty,
  updatedHeight: HeightUpdate = HeightUpdate.NotChanged
)

object StatusUpdate {

  implicit val statusUpdateSemigroup: Semigroup[StatusUpdate] = { (x: StatusUpdate, y: StatusUpdate) =>
    StatusUpdate(
      newStatus = y.newStatus,
      updatedBalances = x.updatedBalances |+| y.updatedBalances,
      requestBalances = x.requestBalances |+| y.requestBalances,
      updatedHeight = y.updatedHeight |+| x.updatedHeight
    )
  }

  sealed trait HeightUpdate extends Product with Serializable

  object HeightUpdate {
    case object NotChanged extends HeightUpdate
    case class Updated(newHeight: Int) extends HeightUpdate
    case class RestartRequired(atHeight: Int) extends HeightUpdate

    implicit val heightUpdateSemigroup: Semigroup[HeightUpdate] = {
      case (NotChanged, y) => y
      case (x, NotChanged) => x
      case (_, y: RestartRequired) => y
      case (x: RestartRequired, _) => x
      case (x: Updated, y: Updated) => Updated(math.max(x.newHeight, y.newHeight))
    }

  }

}
