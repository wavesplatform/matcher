package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.domain.asset.Asset

case class AddressBalanceUpdates(
  regular: Map[Asset, Long], // TODO DEX-1058
  outgoingLeasing: Option[Long],
  pessimisticCorrection: Map[Asset, Long] // TODO DEX-1058
) {

  def changedAssets: Set[Asset] =
    regular.keySet ++
    outgoingLeasing.fold(Set.empty[Asset])(_ => Set(Asset.Waves)) ++
    pessimisticCorrection.keySet

  def nonEmpty: Boolean = regular.nonEmpty || outgoingLeasing.nonEmpty || pessimisticCorrection.nonEmpty
  def isEmpty: Boolean = regular.isEmpty && outgoingLeasing.isEmpty && pessimisticCorrection.isEmpty
}

object AddressBalanceUpdates {

  implicit val accountBalanceUpdatesMonoid: Monoid[AddressBalanceUpdates] = new Monoid[AddressBalanceUpdates] {
    override val empty: AddressBalanceUpdates = AddressBalanceUpdates(Map.empty, None, Map.empty)

    override def combine(x: AddressBalanceUpdates, y: AddressBalanceUpdates): AddressBalanceUpdates = AddressBalanceUpdates(
      regular = x.regular ++ y.regular,
      outgoingLeasing = y.outgoingLeasing.orElse(x.outgoingLeasing),
      pessimisticCorrection = x.pessimisticCorrection ++ y.pessimisticCorrection
    )

  }

  val empty = accountBalanceUpdatesMonoid.empty

}
