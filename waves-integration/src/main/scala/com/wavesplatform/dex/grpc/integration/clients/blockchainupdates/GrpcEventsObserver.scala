package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.status.WavesNodeEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{Status, StatusRuntimeException}
import monix.reactive.Observer

class GrpcEventsObserver(
  subject: Observer[WavesNodeEvent],
  toEvent: BlockchainUpdated => Option[WavesNodeEvent],
  isClosing: => Boolean,
  doOnError: Throwable => Unit
) extends ClientResponseObserver[Empty, SubscribeEvent]
    with AutoCloseable
    with ScorexLogging {

  private var requestStream: ClientCallStreamObserver[Empty] = _

  override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = this.requestStream = requestStream

  override def onNext(value: SubscribeEvent): Unit = value.update.flatMap(toEvent) match {
    case Some(value) => subject.onNext(value)
    case None => log.warn(s"Can't convert to event: $value")
  }

  override def onError(e: Throwable): Unit = {
    log.warn("Got an error", e)
    if (!isClosing) doOnError(e)
  }

  override def close(): Unit = if (requestStream != null) {
    requestStream.cancel("Shutting down", new StatusRuntimeException(Status.CANCELLED))
    log.info("Closed")
  }

  override def onCompleted(): Unit = if (requestStream != null) {
    requestStream.onCompleted()
    log.info("Completed")
  }

}
