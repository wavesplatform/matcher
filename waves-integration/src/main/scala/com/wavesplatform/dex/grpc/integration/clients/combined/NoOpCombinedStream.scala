package com.wavesplatform.dex.grpc.integration.clients.combined

import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.statuses.CombinedStreamStatus
import monix.reactive.Observable

object NoOpCombinedStream extends CombinedStream {
  override def startFrom(height: Int): Unit = {}
  override def restart(): Unit = {}
  override def currentStatus: CombinedStreamStatus = CombinedStreamStatus.Closing(blockchainUpdates = true, utxEvents = true)
  override def updateProcessedHeight(height: Int): Unit = {}
  override def currentProcessedHeight: Int = 0
  override val stream: Observable[WavesNodeEvent] = Observable.empty
}
