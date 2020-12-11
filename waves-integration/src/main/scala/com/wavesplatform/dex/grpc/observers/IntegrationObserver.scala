package com.wavesplatform.dex.grpc.observers

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{Status, StatusRuntimeException}
import monix.reactive.Observer

/**
 * 1. Pushes events to monix observer
 * 2. Do it on demand (requestNext)
 * 3. Is able to close the connection manually (close)
 * 4. Has useful callbacks
 */
class IntegrationObserver[EventT](dest: Observer[EventT], doOnReady: () => Unit, doOnError: Throwable => Unit)
    extends ClientResponseObserver[Empty, EventT]
    with AutoCloseable
    with ScorexLogging {

  private var requestStream: ClientCallStreamObserver[Empty] = _
  private val awaitNext = new AtomicBoolean(true)
  private val buffer = new ConcurrentLinkedQueue[EventT]()

  override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = {
    this.requestStream = requestStream
    requestStream.setOnReadyHandler(() => doOnReady())
  }

  override def onNext(value: EventT): Unit = {
    buffer.add(value)
    tryPublish()
  }

  def requestNext(): Unit = if (awaitNext.compareAndSet(false, true)) tryPublish()

  private def tryPublish(): Unit = if (awaitNext.compareAndSet(true, false)) Option(buffer.poll()) match {
    case Some(x) => dest.onNext(x)
    case None => awaitNext.set(true)
  }

  override def onError(e: Throwable): Unit = doOnError(e)

  override def onCompleted(): Unit = if (requestStream != null) log.info("Completed") // hmm....

  override def close(): Unit = if (requestStream != null) requestStream.cancel("Closing", new StatusRuntimeException(Status.CANCELLED))

}
