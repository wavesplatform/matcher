package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import cats.syntax.option._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.observers.IntegrationObserver
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeEvent, SubscribeRequest}
import io.grpc.stub.ClientCalls
import io.grpc.{CallOptions, ClientCall, Grpc, ManagedChannel}
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

// TODO DEX-999
/*
  From the docs of reactive streams: the grammar must still be respected: (onNext)* (onComplete | onError)
  On error we just restart the stream, so r receives updates from a new stream. That is why we don't propagate errors to r
 */
class GrpcBlockchainUpdatesControlledStream(channel: ManagedChannel)(implicit scheduler: Scheduler)
    extends BlockchainUpdatesControlledStream
    with ScorexLogging {
  @volatile private var grpcObserver: Option[BlockchainUpdatesObserver] = None

  // https://github.com/monix/monix/issues/1019#issuecomment-529700466
  private val internalStream = ConcurrentSubject.publish[SubscribeEvent]
  override val stream: Observable[SubscribeEvent] = internalStream

  private val internalSystemStream = ConcurrentSubject.publish[SystemEvent]
  override val systemStream: Observable[SystemEvent] = internalSystemStream

  override def startFrom(height: Int): Unit = {
    require(height >= 1, "We can not get blocks on height <= 0")
    log.info("Connecting to Blockchain events stream")

    val call = channel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT.withWaitForReady()) // TODO DEX-1001
    val observer = new BlockchainUpdatesObserver(call, height)
    grpcObserver = observer.some
    ClientCalls.asyncServerStreamingCall(call, new SubscribeRequest(height), observer)
  }

  override def requestNext(): Unit = grpcObserver.foreach(_.requestNext())

  override def stop(): Unit = if (grpcObserver.nonEmpty) {
    log.info("Stopping balance updates stream")
    stopGrpcObserver()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
  }

  override def close(): Unit = {
    log.info("Closing balance updates stream")
    stopGrpcObserver()
    internalStream.onComplete()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Closed)
    internalSystemStream.onComplete()
  }

  private def stopGrpcObserver(): Unit = {
    grpcObserver.foreach(_.close())
    grpcObserver = None
  }

  private class BlockchainUpdatesObserver(call: ClientCall[SubscribeRequest, SubscribeEvent], startHeight: Int)
      extends IntegrationObserver[SubscribeRequest, SubscribeEvent](internalStream) {

    override def onReady(): Unit = {
      log.info(
        s"Getting blockchain events from ${Option(call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)).getOrElse("unknown")} starting from $startHeight"
      )
      internalSystemStream.onNext(ControlledStream.SystemEvent.BecameReady)
    }

    override def onError(e: Throwable): Unit = if (!isClosed) {
      log.warn(s"Got an error in blockchain events", e)
      internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
    }

    override def onCompleted(): Unit = log.error("Unexpected onCompleted")
  }

}
