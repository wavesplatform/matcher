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

  private def doOnError(e: Throwable): Unit = {
    log.warn(s"Got an error in blockchain events", e)
    val fromHeight = math.max(1, checkpointHeight - 1)
    scheduler.scheduleOnce(100.millis) { // TODO to config
      subject.onNext(WavesNodeEvent.SyncFailed(fromHeight))
      startFrom(fromHeight) // Because already stopped
    }
  }

  private def stopGrpcObserver(): Unit = grpcObserver.foreach(_.onCompleted())

}
