package com.wavesplatform.dex.grpc.integration.clients.matcherext

import cats.syntax.option._
import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream
import com.wavesplatform.dex.grpc.integration.clients.ControlledStream.SystemEvent
import com.wavesplatform.dex.grpc.integration.services.{UtxEvent, WavesBlockchainApiGrpc}
import io.grpc.stub.{ClientCallStreamObserver, ClientCalls, ClientResponseObserver}
import io.grpc.{CallOptions, Grpc, ManagedChannel}
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.util.chaining._

class UtxEventsControlledStream(channel: ManagedChannel)(implicit scheduler: Scheduler) extends ControlledStream[UtxEvent] with ScorexLogging {
  @volatile private var grpcObserver: Option[ClientResponseObserver[Empty, UtxEvent]] = None

  private val internalStream = ConcurrentSubject.publish[UtxEvent]
  override val stream: Observable[UtxEvent] = internalStream

  private val internalSystemStream = ConcurrentSubject.publish[SystemEvent]
  override val systemStream: Observable[ControlledStream.SystemEvent] = internalSystemStream

  private val empty: Empty = Empty()

  def start(): Unit = {
    val call = channel.newCall(WavesBlockchainApiGrpc.METHOD_GET_UTX_EVENTS, CallOptions.DEFAULT.withWaitForReady()) // TODO DEX-1001

    val observer = new ClientResponseObserver[Empty, UtxEvent] {
      // TODO requestStream close !!!!!!!!!!!!!!
      override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = requestStream.setOnReadyHandler { () =>
        log.info(s"Getting utx events from ${call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)}")
        internalSystemStream.onNext(ControlledStream.SystemEvent.BecameReady)
      }

      override def onNext(value: UtxEvent): Unit = internalStream.onNext(value)

      override def onError(e: Throwable): Unit = {
        log.warn(s"Got an error in utx events", e)
        internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
      }

      override def onCompleted(): Unit = internalSystemStream.onNext(ControlledStream.SystemEvent.Closed) // hmmmm ?
    }.tap(x => grpcObserver = x.some)

    ClientCalls.asyncServerStreamingCall(call, empty, observer)
  }

  override def stop(): Unit = {
    log.info("Closing utx events stream")
    stopGrpcObserver()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Stopped)
  }

  override def close(): Unit = {
    log.info("Stopping utx events stream")
    internalStream.onComplete()
    internalSystemStream.onNext(ControlledStream.SystemEvent.Closed)
    stopGrpcObserver()
  }

  private def stopGrpcObserver(): Unit = {
    grpcObserver.foreach(_.onCompleted())
    grpcObserver = None
  }
}
