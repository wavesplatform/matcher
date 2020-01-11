package com.wavesplatform.dex.grpc.integration

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.grpc.integration.clients.{WavesBlockchainCachingClient, WavesBlockchainClient, WavesBlockchainGrpcAsyncClient}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.internal.DnsNameResolverProvider
import io.grpc.netty.NettyChannelBuilder
import io.netty.channel.ChannelOption
import io.netty.channel.nio.NioEventLoopGroup
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

object WavesBlockchainClientBuilder extends ScorexLogging {
  def async(target: String,
            defaultCachesExpiration: FiniteDuration,
            monixScheduler: Scheduler,
            grpcExecutionContext: ExecutionContext): WavesBlockchainClient[Future] = {
    log.info(s"Building gRPC client for server: $target")

    val eventLoopGroup = new NioEventLoopGroup
    val channel = NettyChannelBuilder
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

    new WavesBlockchainCachingClient(
      new WavesBlockchainGrpcAsyncClient(eventLoopGroup, channel, monixScheduler)(grpcExecutionContext),
      defaultCachesExpiration,
      monixScheduler
    )(grpcExecutionContext)
  }
}
