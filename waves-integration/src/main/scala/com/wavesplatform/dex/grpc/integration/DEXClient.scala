package com.wavesplatform.dex.grpc.integration

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.grpc.integration.clients.{WavesBlockchainCachingClient, WavesBlockchainGrpcAsyncClient}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.internal.DnsNameResolverProvider
import io.grpc.netty.NettyChannelBuilder
import io.netty.channel.ChannelOption
import io.netty.channel.nio.NioEventLoopGroup
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, blocking}

class DEXClient(target: String, defaultCachesExpiration: FiniteDuration)(implicit val monixScheduler: Scheduler,
                                                                         val grpcExecutionContext: ExecutionContext)
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

  lazy val wavesBlockchainAsyncClient = new WavesBlockchainCachingClient(
    new WavesBlockchainGrpcAsyncClient(channel),
    defaultCachesExpiration
  )

  def close(): Future[Unit] = Future(blocking(eventLoopGroup.shutdownGracefully(0, 500, TimeUnit.MILLISECONDS)))
}
