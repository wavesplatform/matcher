package com.wavesplatform.dex.grpc.integration.clients.state

import cats.syntax.semigroup._
import com.wavesplatform.dex.collection.MapOps.Ops
import com.wavesplatform.dex.domain.account.Address
import com.wavesplatform.dex.domain.asset.Asset

import scala.collection.View

case class WavesFork(history: List[WavesBlock], actual: BlockchainBalance) {

  def latestBlock: Option[WavesBlock] = history.headOption

  def withBlock(block: WavesBlock): WavesFork = WavesFork(
    history = block :: history,
    actual = actual |+| block.changes
  )

  def blocksFrom(from: BlockRef): View[WavesBlock] = history.view.takeWhile(_.blockInfo.height > from.height)

}

object WavesFork {

  implicit final class WavesBlocksViewSliceOps(val self: View[WavesBlock]) extends AnyVal {

    def changedAddresses: Map[Address, AddressChanges] = self.map { block =>
      block.changes.outLeases.map
    }

  }

}
