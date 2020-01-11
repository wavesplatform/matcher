package com.wavesplatform.dex.grpc.integration

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.grpc.integration.clients.{WavesBlockchainCachingClient, WavesBlockchainGrpcAsyncClient}
import com.wavesplatform.dex.grpc.integration.effect.Implicits.NettyFutureOps
import com.wavesplatform.utils.ScorexLogging
import io.grpc.internal.DnsNameResolverProvider
import io.grpc.netty.NettyChannelBuilder
import io.netty.channel.ChannelOption
import io.netty.channel.nio.NioEventLoopGroup
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

// TODO refactor to Builder
/**
  * @param monixScheduler Is not an implicit, because it is ExecutionContext too
  */
class DEXClient(target: String, defaultCachesExpiration: FiniteDuration, monixScheduler: Scheduler)(
    implicit private val grpcExecutionContext: ExecutionContext)
    extends ScorexLogging {

  log.info(s"NODE gRPC server: $target")

  private val eventLoopGroup = new NioEventLoopGroup

  private val channel =
    NettyChannelBuilder
      .forTarget(target)
      .maxHedgedAttempts(10)
      .maxRetryAttempts(20)
      .keepAliveWithoutCalls(true)
      .keepAliveTime(2, TimeUnit.SECONDS)
      .keepAliveTimeout(5, TimeUnit.SECONDS)
      .idleTimeout(7, TimeUnit.SECONDS)
      .nameResolverFactory(new DnsNameResolverProvider)
      .defaultLoadBalancingPolicy("pick_first")
      .executor(grpcExecutionContext.execute)
      .eventLoopGroup(eventLoopGroup)
      .withOption[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, 5000)
      .usePlaintext()
      .build

  val wavesBlockchainAsyncClient = new WavesBlockchainCachingClient(
    new WavesBlockchainGrpcAsyncClient(channel, monixScheduler),
    defaultCachesExpiration,
    monixScheduler
  )

  def close(): Future[Unit] = {
    channel.shutdownNow()
    eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS).asScala.map(_ => ())
  }
}
