package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain.WavesNodeEvent
import com.wavesplatform.events.api.grpc.protobuf.{SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{ClientCall, Grpc, Status, StatusRuntimeException}
import monix.reactive.Observer

class GrpcBlockchainEventsObserver(
  subject: Observer[WavesNodeEvent],
  toEvent: BlockchainUpdated => Option[WavesNodeEvent],
  isClosing: => Boolean,
  call: ClientCall[SubscribeRequest, SubscribeEvent],
  doOnError: Throwable => Unit
) extends ClientResponseObserver[Empty, SubscribeEvent]
    with AutoCloseable
    with ScorexLogging {

  private var requestStream: ClientCallStreamObserver[Empty] = _
  private val awaitNext = new AtomicBoolean(true)
  private val buffer = new ConcurrentLinkedQueue[WavesNodeEvent]()

  override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = {
    this.requestStream = requestStream
    requestStream.setOnReadyHandler(() => log.info(s"Getting blockchain events from ${call.getAttributes.get(Grpc.TRANSPORT_ATTR_REMOTE_ADDR)}"))
  }

  override def onNext(value: SubscribeEvent): Unit = value.update.flatMap(toEvent) match {
    case None => log.warn(s"Can't convert to event: $value")
    case Some(value) =>
      buffer.add(value)
      tryPublish()
  }

  def requestNext(): Unit = if (awaitNext.compareAndSet(false, true)) tryPublish()

  private def tryPublish(): Unit = if (awaitNext.compareAndSet(true, false)) Option(buffer.poll()) match {
    case Some(x) => subject.onNext(x)
    case None => awaitNext.set(true)
  }

  override def onError(e: Throwable): Unit = if (!isClosing) doOnError(e)

  override def onCompleted(): Unit = if (requestStream != null) log.info("Completed")

  override def close(): Unit = if (requestStream != null) requestStream.cancel("Shutting down", new StatusRuntimeException(Status.CANCELLED))

}
