package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observer

import scala.concurrent.duration.DurationInt

class GrpcBlockchainUpdatesClientStreamControl(
  subject: Observer[WavesNodeEvent],
  subscribe: (Observer[WavesNodeEvent], Int, Throwable => Unit) => Option[StreamObserver[SubscribeEvent]]
)(implicit scheduler: Scheduler)
    extends BlockchainUpdatesStreamControl
    with ScorexLogging {
  @volatile private var grpcObserver: Option[StreamObserver[SubscribeEvent]] = None

  // Note, it is not processed height! A less value could be emitted to control
  @volatile private var checkpointHeight = 1

  def startFrom(height: Int): Unit = {
    require(height >= 1, "We can not get blocks on height <= 0")
    log.info(s"Starting from $height")
    checkpointHeight = height - 1
    grpcObserver = subscribe(subject, height, doOnError)
  }

  override def restartFrom(height: Int): Unit = {
    stopGrpcObserver()
    startFrom(height)
  }

  override def checkpoint(height: Int): Unit = checkpointHeight = height

  override def stop(): Unit = {
    log.info("Stopping balance updates stream")
    stopGrpcObserver()
    subject.onComplete()
  }

  // Happens only during disconnects or other gRPC issues
  private def doOnError(e: Throwable): Unit = {
    // TODO probably we need something like backpressure to no process invalid preloaded blocks after SyncFailed
    log.warn(s"Got an error in blockchain events", e)
    val requestHeight = checkpointHeight
    val rollBackHeight = math.max(0, checkpointHeight - 1)
    // It needs to be before the delay to obtain the UTX stream connection before blocks
    subject.onNext(WavesNodeEvent.RolledBack(WavesNodeEvent.RolledBack.To.Height(rollBackHeight)))
    // TODO need to stop?
    scheduler.scheduleOnce(50.millis) { // TODO to config
      // TODO
      // 1. prepend SyncFailed only if started
      // 2. we need not only wait for a connection, but for same height too!
      startFrom(requestHeight) // Not stopping grpcObserver, because it is already stopped
    }
  }

  private def stopGrpcObserver(): Unit = grpcObserver.foreach(_.onCompleted())

}
