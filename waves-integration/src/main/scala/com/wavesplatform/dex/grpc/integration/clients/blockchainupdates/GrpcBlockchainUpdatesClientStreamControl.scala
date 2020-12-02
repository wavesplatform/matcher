package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent
import monix.execution.Scheduler
import monix.reactive.Observer

import scala.concurrent.duration.DurationInt

// TODO DEX-999
class GrpcBlockchainUpdatesClientStreamControl(
  subject: Observer[WavesNodeEvent],
  subscribe: (Observer[WavesNodeEvent], Int, Throwable => Unit) => Option[GrpcBlockchainEventsObserver]
)(implicit scheduler: Scheduler)
    extends BlockchainUpdatesStreamControl
    with ScorexLogging {
  @volatile private var grpcObserver: Option[GrpcBlockchainEventsObserver] = None

  // Note, it is not processed height! A less value could be emitted to control
  @volatile private var checkpointHeight = 1

  def startFrom(height: Int): Unit = {
    require(height >= 1, "We can not get blocks on height <= 0")
    checkpointHeight = height - 1
    grpcObserver = subscribe(subject, height, doOnError)
  }

  override def restartFrom(height: Int): Unit = {
    stopGrpcObserver()
    startFrom(height)
  }

  override def requestNext(): Unit = grpcObserver.foreach(_.requestNext())

  override def checkpoint(height: Int): Unit = checkpointHeight = height

  override def stop(): Unit = {
    log.info("Stopping balance updates stream")
    stopGrpcObserver()
    subject.onComplete()
  }

  // Happens only during disconnects or other gRPC issues
  private def doOnError(e: Throwable): Unit = {
    log.warn(s"Got an error in blockchain events", e)
    val requestHeight = checkpointHeight
    val rollBackHeight = math.max(0, checkpointHeight - 1)
    // It needs to be before the delay to obtain the UTX stream connection before blocks
    subject.onNext(WavesNodeEvent.RolledBack(WavesNodeEvent.RolledBack.To.Height(rollBackHeight)))
    scheduler.scheduleOnce(50.millis) { // TODO DEX-1000
      startFrom(requestHeight) // Not stopping grpcObserver, because it is already stopped
    }
  }

  private def stopGrpcObserver(): Unit = grpcObserver.foreach(_.onCompleted())

}
