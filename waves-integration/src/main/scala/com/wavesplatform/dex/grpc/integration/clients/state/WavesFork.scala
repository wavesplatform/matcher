package com.wavesplatform.dex.grpc.integration.clients.state

import cats.syntax.semigroup._

import scala.collection.View

case class WavesFork(history: List[WavesBlock], actual: BlockchainBalance) {

  def latestBlock: Option[WavesBlock] = history.headOption

  def withBlock(block: WavesBlock): WavesFork = WavesFork(
    history = block :: history,
    actual = actual |+| block.changes
  )

  def blocksFrom(from: BlockRef): View[WavesBlock] = history.view.takeWhile(_.blockInfo.height > from.height)

}
