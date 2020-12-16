package com.wavesplatform.dex.grpc.integration.clients.matcherext

import java.util.concurrent.atomic.AtomicBoolean

import cats.syntax.option._
import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
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

  private val internalStream = ConcurrentSubject.publish[UtxEvent] // .replayLimited[UtxEvent](10)
  // HACK: Monix skips the first few messages! So we have to turn it into a hot
  override val stream: Observable[UtxEvent] = internalStream // .publish

  private val internalSystemStream = ConcurrentSubject.publish[SystemEvent] // .replayLimited[SystemEvent](10)
  override val systemStream: Observable[SystemEvent] = internalSystemStream // .publish

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
    internalSystemStream.onNext(SystemEvent.Stopped)
  }

  override def close(): Unit = {
    log.info("Closing utx events stream")
    stopGrpcObserver()
    internalStream.onComplete()
    internalSystemStream.onNext(SystemEvent.Closed)
    internalSystemStream.onComplete()
  }

  private def stopGrpcObserver(): Unit = {
    grpcObserver.foreach(_.close())
    grpcObserver = None
  }

  private class UtxEventObserver(call: ClientCall[Empty, UtxEvent]) extends ClosingObserver[Empty, UtxEvent] {

    private val ready = new AtomicBoolean(false)

    override def onReady(): Unit = {
      val address = Option(call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)).fold("unknown")(_.toString)
      log.info(s"Getting utx events from $address")
    }

    override def onNext(value: UtxEvent): Unit = {
      internalStream.onNext(value)

      // To guarantee UtxSwitch before readiness and thus before resolving a temporary rollback (StatusTransitions)
      if (ready.compareAndSet(false, true)) {
        if (!value.`type`.isSwitch) log.error(s"Expected UtxSwitch, received: ${value.`type`}")
        internalSystemStream.onNext(SystemEvent.BecameReady)
      }
    }

    override def onError(e: Throwable): Unit = if (!isClosed) {
      log.warn(s"Got an error in utx events", e)
      internalSystemStream.onNext(SystemEvent.Stopped)
    }

    override def onCompleted(): Unit = log.error("Unexpected onCompleted")
  }

}
