package com.wavesplatform.dex.grpc.integration.clients.state

import cats.Monoid
import com.wavesplatform.dex.domain.asset.Asset

case class AddressChanges(regular: Set[Asset], outLease: Boolean)

object AddressChanges {

  implicit val addressChangesMonoid: Monoid[AddressChanges] = new Monoid[AddressChanges] {
    override val empty = AddressChanges(Set.empty, outLease = false)

    override def combine(x: AddressChanges, y: AddressChanges): AddressChanges =
      AddressChanges(
        regular = x.regular ++ y.regular,
        outLease = x.outLease || y.outLease
      )

  }

}
