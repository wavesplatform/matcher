package com.wavesplatform.dex.grpc.integration.clients.state

import cats.Monoid
import com.wavesplatform.dex.domain.asset.Asset

case class AddressChangesIndex(regular: Set[Asset], outLease: Boolean)

object AddressChangesIndex {

  implicit val addressChangesMonoid: Monoid[AddressChangesIndex] = new Monoid[AddressChangesIndex] {
    override val empty = AddressChangesIndex(Set.empty, outLease = false)

    override def combine(x: AddressChangesIndex, y: AddressChangesIndex): AddressChangesIndex =
      AddressChangesIndex(
        regular = x.regular ++ y.regular,
        outLease = x.outLease || y.outLease
      )

  }

}
