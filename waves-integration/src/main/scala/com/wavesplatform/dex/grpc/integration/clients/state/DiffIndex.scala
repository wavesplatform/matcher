package com.wavesplatform.dex.grpc.integration.clients.state

import cats.Monoid
import cats.instances.map._
import cats.instances.set._
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

case class DiffIndex(regular: Map[Address, Set[Asset]], outLeases: Set[Address]) {

  def without(that: DiffIndex): DiffIndex = DiffIndex(
    regular = regular -- that.regular.keySet,
    outLeases = outLeases -- that.outLeases
  )

  def isEmpty: Boolean = regular.isEmpty && outLeases.isEmpty

}

object DiffIndex {

  implicit val diffIndexMonoid: Monoid[DiffIndex] = new Monoid[DiffIndex] {
    override val empty = DiffIndex(Map.empty, Set.empty)

    override def combine(x: DiffIndex, y: DiffIndex): DiffIndex = DiffIndex(
      regular = x.regular |+| y.regular,
      outLeases = x.outLeases |+| y.outLeases
    )

  }

}
