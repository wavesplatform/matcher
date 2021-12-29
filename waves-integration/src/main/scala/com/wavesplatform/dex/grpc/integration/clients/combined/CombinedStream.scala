package com.wavesplatform.dex.grpc.integration.clients.combined

import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.dex.statuses.CombinedStreamStatus
import monix.reactive.Observable

import scala.concurrent.duration.FiniteDuration

trait CombinedStream {
  def startFrom(height: Int): Unit
  def restart(): Unit

  def currentStatus: CombinedStreamStatus

  def updateProcessedHeight(height: Int): Unit
  def currentProcessedHeight: Int

  val stream: Observable[WavesNodeEvent]
}

object CombinedStream {

  case class Settings(restartDelay: FiniteDuration)

}
