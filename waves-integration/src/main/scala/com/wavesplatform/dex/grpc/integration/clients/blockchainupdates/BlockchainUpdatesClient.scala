package com.wavesplatform.dex.grpc.integration.clients.blockchainupdates

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.dex.grpc.integration.tool.RestartableManagedChannel
import io.netty.channel.EventLoopGroup
import monix.execution.Scheduler

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
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

class DefaultBlockchainUpdatesClient(
  eventLoopGroup: EventLoopGroup,
  channel: RestartableManagedChannel,
  monixScheduler: Scheduler,
  noDataTimeout: FiniteDuration
)(implicit grpcExecutionContext: ExecutionContext)
    extends BlockchainUpdatesClient
    with ScorexLogging {

  override val blockchainEvents = new GrpcBlockchainUpdatesControlledStream(channel, noDataTimeout)(monixScheduler)

  override def close(): Future[Unit] = {
    blockchainEvents.close()
    channel.shutdown(500.millis)
    // TODO DEX-998
    if (eventLoopGroup.isShuttingDown) Future.successful(())
    else eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }

}
