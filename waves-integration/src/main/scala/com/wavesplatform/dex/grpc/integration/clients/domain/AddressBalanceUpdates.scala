package com.wavesplatform.dex.grpc.integration.clients.domain

import cats.Monoid
import com.wavesplatform.dex.domain.asset.Asset

case class AddressBalanceUpdates(
  regular: Map[Asset, Long],
  outLease: Option[Long],
  pessimisticCorrection: Map[Asset, Long]
) {

  def changedAssets: Set[Asset] =
    regular.keySet ++
    outLease.fold(Set.empty[Asset])(_ => Set(Asset.Waves)) ++
    pessimisticCorrection.keySet

  def nonEmpty: Boolean = regular.nonEmpty || outLease.nonEmpty || pessimisticCorrection.nonEmpty

}

object AddressBalanceUpdates {

  implicit val accountBalanceUpdatesMonoid: Monoid[AddressBalanceUpdates] = new Monoid[AddressBalanceUpdates] {
    override val empty: AddressBalanceUpdates = AddressBalanceUpdates(Map.empty, None, Map.empty)

    override def combine(x: AddressBalanceUpdates, y: AddressBalanceUpdates): AddressBalanceUpdates = AddressBalanceUpdates(
      regular = x.regular ++ y.regular,
      outLease = y.outLease.orElse(x.outLease),
      pessimisticCorrection = x.pessimisticCorrection ++ y.pessimisticCorrection
    )

  }

}
