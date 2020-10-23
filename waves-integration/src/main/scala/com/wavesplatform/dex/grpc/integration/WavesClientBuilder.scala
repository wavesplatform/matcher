package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.{MatcherExtensionCachingClient, MatcherExtensionClient, MatcherExtensionGrpcAsyncClient}
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import io.grpc.ManagedChannel
import io.grpc.internal.DnsNameResolverProvider
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.execution.Scheduler

import scala.concurrent.{ExecutionContext, Future}

object WavesClientBuilder extends ScorexLogging {

  def asyncMatcherExtension(
    wavesBlockchainClientSettings: WavesBlockchainClientSettings,
    monixScheduler: Scheduler,
    grpcExecutionContext: ExecutionContext
  ): MatcherExtensionClient[Future] = {

    log.info(s"Building Matcher Extension gRPC client for server: ${wavesBlockchainClientSettings.grpc.target}")

    val eventLoopGroup = new NioEventLoopGroup

    val channel: ManagedChannel =
      wavesBlockchainClientSettings.grpc.toNettyChannelBuilder
        .nameResolverFactory(new DnsNameResolverProvider)
        .executor((command: Runnable) => grpcExecutionContext.execute(command))
        .eventLoopGroup(eventLoopGroup)
        .channelType(classOf[NioSocketChannel])
        .usePlaintext()
        .build

    new MatcherExtensionCachingClient(
      new MatcherExtensionGrpcAsyncClient(eventLoopGroup, channel, monixScheduler)(grpcExecutionContext),
      wavesBlockchainClientSettings.defaultCachesExpiration
    )(grpcExecutionContext)
  }

}
