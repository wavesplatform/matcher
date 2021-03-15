package com.wavesplatform.dex.grpc.observers

import java.util.concurrent.atomic.AtomicBoolean

import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{Status, StatusRuntimeException}

/**
 * Use it only for streams those receive the data, but not send it. See comments.
 */
trait ClosingObserver[ArgT, EventT] extends ClientResponseObserver[ArgT, EventT] {
  private val closed = new AtomicBoolean(false)
  private var requestStream: ClientCallStreamObserver[ArgT] = _

  override def beforeStart(requestStream: ClientCallStreamObserver[ArgT]): Unit = {
    this.requestStream = requestStream
    onReady()
    // requestStream.setOnReadyHandler // This works only for streams which send the data, see CallStreamObserver.isReady
  }

  def onReady(): Unit = {}

  def isClosed: Boolean = closed.get()

  def close(): Unit = if (closed.compareAndSet(false, true)) requestStream.cancel("Closing", new StatusRuntimeException(Status.CANCELLED))
}
