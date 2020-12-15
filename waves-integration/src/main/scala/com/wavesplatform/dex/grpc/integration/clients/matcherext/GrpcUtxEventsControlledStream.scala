package com.wavesplatform.dex.grpc.integration.clients.matcherext

import cats.syntax.option._
import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.services.{UtxEvent, WavesBlockchainApiGrpc}
import com.wavesplatform.dex.grpc.observers.ClosingObserver
import io.grpc._
import io.grpc.stub.ClientCalls
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

/**
 * @see GrpcBlockchainUpdatesControlledStream
 */
class GrpcUtxEventsControlledStream(channel: ManagedChannel)(implicit scheduler: Scheduler)
    extends UtxEventsControlledStream
    with ScorexLogging {
  @volatile private var grpcObserver: Option[UtxEventObserver] = None

  private val internalStream = ConcurrentSubject.publish[UtxEvent]
  override val stream: Observable[UtxEvent] = internalStream

  private val internalSystemStream = ConcurrentSubject.publish[SystemEvent]
  override val systemStream: Observable[ControlledStream.SystemEvent] = internalSystemStream

  private val empty: Empty = Empty()

  override def start(): Unit = {
    log.info("Connecting to UTX stream")
    val call = channel.newCall(WavesBlockchainApiGrpc.METHOD_GET_UTX_EVENTS, CallOptions.DEFAULT.withWaitForReady()) // TODO DEX-1001
    val observer = new UtxEventObserver(call)
    grpcObserver = observer.some
    ClientCalls.asyncServerStreamingCall(call, empty, observer)
  }

  override def stop(): Unit = if (grpcObserver.nonEmpty) {
    log.info("Stopping utx events stream")
    stopGrpcObserver()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
  }

  override def close(): Unit = {
    log.info("Closing utx events stream")
    stopGrpcObserver()
    internalStream.onComplete()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Closed)
    internalSystemStream.onComplete()
  }

  private def stopGrpcObserver(): Unit = {
    grpcObserver.foreach(_.close())
    grpcObserver = None
  }

  private class UtxEventObserver(call: ClientCall[Empty, UtxEvent]) extends ClosingObserver[Empty, UtxEvent] {

    override def onReady(): Unit = {
      internalSystemStream.onNext(ControlledStream.SystemEvent.BecameReady)
      val address = Option(call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)).fold("unknown")(_.toString)
      log.info(s"Getting utx events from $address")
    }

    override def onNext(value: UtxEvent): Unit = internalStream.onNext(value)

    override def onError(e: Throwable): Unit = if (!isClosed) {
      log.warn(s"Got an error in utx events", e)
      internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
    }

    override def onCompleted(): Unit = log.error("Unexpected onCompleted")
  }

}
