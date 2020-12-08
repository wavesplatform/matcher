package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import java.util.concurrent.TimeUnit

import cats.syntax.option._
import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.domain._
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.events.api.grpc.protobuf.{BlockchainUpdatesApiGrpc, SubscribeRequest}
import io.grpc.stub.ClientCalls
import io.grpc.{CallOptions, ManagedChannel}
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.{ExecutionContext, Future}

trait BlockchainUpdatesClient {
  /**
   * The extension guarantees:
   * 1. During initialization: events will be sent after initial blocks
   * 2. Events has causal ordering
   */
  def blockchainEvents(fromHeight: Int): (Observable[WavesNodeEvent], BlockchainUpdatesStreamControl)
  def close(): Future[Unit]
}

class DefaultBlockchainUpdatesClient(eventLoopGroup: EventLoopGroup, channel: ManagedChannel, monixScheduler: Scheduler)(implicit
  grpcExecutionContext: ExecutionContext
) extends BlockchainUpdatesClient
    with ScorexLogging {
  @volatile private var isClosing = false

  override def blockchainEvents(fromHeight: Int): (Observable[WavesNodeEvent], BlockchainUpdatesStreamControl) = {
    // From the docs: the grammar must still be respected: (onNext)* (onComplete | onError)
    // On error we just restart the stream, so r receives updates from a new stream. That is why we don't propagate errors to r
    val subject = ConcurrentSubject.publish[WavesNodeEvent](monixScheduler)
    val control = new GrpcBlockchainUpdatesClientStreamControl(
      subject,
      subscribe = { (subject, fromHeight, onError) =>
        if (isClosing) none
        else {
          log.debug(s"Subscribed on blockchain events from $fromHeight")
          val call = channel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT.withWaitForReady()) // TODO DEX-1001
          val r = new GrpcBlockchainEventsObserver(subject, Conversions.toEvent, isClosing, call, onError)

          ClientCalls.asyncServerStreamingCall(call, new SubscribeRequest(fromHeight), r)
          r.some
        }
      }
    )(monixScheduler)
    control.startFrom(fromHeight)
    (subject, control)
  }

  override def close(): Future[Unit] = {
    isClosing = true
    channel.shutdown()
    channel.awaitTermination(500, TimeUnit.MILLISECONDS)
    // TODO DEX-998
    if (eventLoopGroup.isShuttingDown) Future.successful(())
    else eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

}
