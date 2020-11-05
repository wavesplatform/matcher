package com.wavesplatform.dex.grpc.integration.clients

import cats.kernel.Monoid
import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.MapOps.Ops
import com.wavesplatform.dex.domain.account.Address
import supertagged._

import scala.collection.View

package object state {
  object WavesBlock extends TaggedType[BaseBlock]
  type WavesBlock = WavesBlock.Type

  object WavesMicroBlock extends TaggedType[BaseBlock]
  type WavesMicroBlock = WavesMicroBlock.Type

  implicit final class WavesBlocksViewSliceOps(val self: View[WavesBlock]) extends AnyVal {

    def changedAddresses: Map[Address, AddressChangesIndex] =
      self.map(_.changes.addressIndexes).foldLeft(Map.empty[Address, AddressChangesIndex]) { case (orig, combine) =>
        orig.deepCombine(combine)(_ |+| _)
      }

    def combinedBlockchainBalance: BlockchainBalance =self.map(_.changes).foldLeft(Monoid.empty[BlockchainBalance])(_ |+| _)

  }

}
