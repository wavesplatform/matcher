package com.wavesplatform.dex.grpc.integration.clients.state

import cats.Monoid
import cats.instances.map._
import cats.instances.set._
import cats.syntax.semigroup._
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

import scala.collection.immutable.Queue

sealed trait BlockchainStatus extends Product with Serializable

object BlockchainStatus {
  case object Init extends BlockchainStatus
  case class Normal(mainFork: WavesFork) extends BlockchainStatus
  case class TransientRollback(commonBlockRef: BlockRef, mainFork: WavesFork, newFork: WavesFork, diffIndex: DiffIndex) extends BlockchainStatus
  // TODO collect diff between forks?
  case class TransientResolving(mainFork: WavesFork, waitInfoFor: Set[Address], stash: Queue[BlockchainEvent]) extends BlockchainStatus

  case class DiffIndex(regular: Map[Address, Set[Asset]], outLeases: Set[Address])

  object DiffIndex {

    implicit val diffIndexMonoid: Monoid[DiffIndex] = new Monoid[DiffIndex] {
      override val empty = DiffIndex(Map.empty, Set.empty)

      override def combine(x: DiffIndex, y: DiffIndex): DiffIndex = DiffIndex(
        regular = x.regular |+| y.regular,
        outLeases = x.outLeases |+| y.outLeases
      )

    }

  }

}
