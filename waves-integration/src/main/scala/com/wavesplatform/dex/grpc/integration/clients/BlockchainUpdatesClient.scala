package com.wavesplatform.dex.grpc.integration.clients

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import com.google.protobuf.empty.Empty
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeEvent, SubscribeRequest}
import com.wavesplatform.events.protobuf.BlockchainUpdated
import io.grpc.stub.{ClientCallStreamObserver, ClientResponseObserver}
import io.grpc.{ManagedChannel, Status, StatusRuntimeException}
import io.netty.channel.EventLoopGroup
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{ExecutionContext, Future}

trait BlockchainUpdatesClient {
  def blockchainEvents(fromHeight: Int): (Observable[BlockchainUpdated], Cancelable)
  def close(): Future[Unit]
}

class DefaultBlockchainUpdatesClient(eventLoopGroup: EventLoopGroup, channel: ManagedChannel, monixScheduler: Scheduler)(implicit
  grpcExecutionContext: ExecutionContext
) extends BlockchainUpdatesClient
    with ScorexLogging {
  private val shuttingDown = new AtomicBoolean(false)
  private val grpcService = BlockchainUpdatesApiGrpc.stub(channel) // TODO move this dependency to constructor?

  override def blockchainEvents(fromHeight: Int): (Observable[BlockchainUpdated], Cancelable) = {
    val r = ConcurrentSubject.publish[BlockchainUpdated](monixScheduler)
    val o = new EventsObserver(r)
    grpcService.subscribe(new SubscribeRequest(fromHeight), o)
    (r, () => o.close())
  }

  override def close(): Future[Unit] = {
    shuttingDown.set(true)
    channel.shutdown()
    channel.awaitTermination(500, TimeUnit.MILLISECONDS)
    // See NettyChannelBuilder.eventLoopGroup
    // TODO can we have one loop group for both clients?
    eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

  final private class EventsObserver(subject: ConcurrentSubject[BlockchainUpdated, BlockchainUpdated])
      extends ClientResponseObserver[Empty, SubscribeEvent]
      with AutoCloseable {

    private var requestStream: ClientCallStreamObserver[Empty] = _

    override def beforeStart(requestStream: ClientCallStreamObserver[Empty]): Unit = this.requestStream = requestStream

    override def onNext(value: SubscribeEvent): Unit =
      value.update.foreach(subject.onNext)

    override def onError(e: Throwable): Unit = if (!shuttingDown.get()) subject.onError(e)

    override def close(): Unit = if (requestStream != null) requestStream.cancel("Shutting down", new StatusRuntimeException(Status.CANCELLED))

    override def onCompleted(): Unit = log.info("Balance changes stream completed!")
  }

}
