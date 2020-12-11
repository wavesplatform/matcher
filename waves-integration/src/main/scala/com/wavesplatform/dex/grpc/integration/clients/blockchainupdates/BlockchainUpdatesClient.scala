package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import io.grpc.ManagedChannel
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler

import scala.concurrent.{ExecutionContext, Future}

trait BlockchainUpdatesClient {

  /**
   * The extension guarantees:
   * 1. During initialization: events will be sent after initial blocks
   * 2. Events has causal ordering
   */
  val blockchainEvents: BlockchainUpdatesControlledStream
  def close(): Future[Unit]
}

class DefaultBlockchainUpdatesClient(eventLoopGroup: EventLoopGroup, channel: ManagedChannel, monixScheduler: Scheduler)(implicit
  grpcExecutionContext: ExecutionContext
) extends BlockchainUpdatesClient
    with ScorexLogging {

  // TODO move docs
  // From the docs: the grammar must still be respected: (onNext)* (onComplete | onError)
  // On error we just restart the stream, so r receives updates from a new stream. That is why we don't propagate errors to r
  override val blockchainEvents: BlockchainUpdatesControlledStream = new BlockchainUpdatesControlledStream(channel)(monixScheduler)

  override def close(): Future[Unit] = {
    blockchainEvents.close()
    channel.shutdown()
    channel.awaitTermination(500, TimeUnit.MILLISECONDS)
    // TODO DEX-998
    if (eventLoopGroup.isShuttingDown) Future.successful(())
    else eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

}
