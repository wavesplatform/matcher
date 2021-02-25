package com.wavesplatform.dex.grpc.observers

import monix.reactive.Observer

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

/**
 * 1. Pushes events to monix observer
 * 2. Do it on demand (requestNext)
 */
abstract class IntegrationObserver[ArgT, EventT](dest: Observer[EventT]) extends ClosingObserver[ArgT, EventT] {

  // Invariant: awaitNext == buffer.isEmpty

  private[observers] val awaitNext = new AtomicBoolean(true)
  private[observers] val buffer = new ConcurrentLinkedQueue[EventT]()

  override def onNext(value: EventT): Unit = {
    buffer.add(value)
    tryPublish()
  }

  def requestNext(): Unit = if (awaitNext.compareAndSet(false, true)) tryPublish()

  private def tryPublish(): Unit = if (awaitNext.compareAndSet(true, false)) Option(buffer.poll()) match {
    case Some(x) => dest.onNext(x)
    case None => awaitNext.set(true)
  }

}
