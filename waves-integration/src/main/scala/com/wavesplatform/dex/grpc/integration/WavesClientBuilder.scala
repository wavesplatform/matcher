package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients._
import com.wavesplatform.dex.grpc.integration.clients.blockchainupdates.DefaultBlockchainUpdatesClient
import com.wavesplatform.dex.grpc.integration.clients.matcherext.{MatcherExtensionCachingClient, MatcherExtensionGrpcAsyncClient}
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import io.grpc.ManagedChannel
import io.grpc.internal.DnsNameResolverProvider
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.execution.Scheduler

import scala.concurrent.ExecutionContext

object WavesClientBuilder extends ScorexLogging {

  def async(
    wavesBlockchainClientSettings: WavesBlockchainClientSettings,
    monixScheduler: Scheduler,
    grpcExecutionContext: ExecutionContext
  ): WavesBlockchainClient = {

    val eventLoopGroup = new NioEventLoopGroup

    log.info(s"Building Matcher Extension gRPC client for server: ${wavesBlockchainClientSettings.grpc.target}")
    val matcherExtensionChannel: ManagedChannel =
      wavesBlockchainClientSettings.grpc.toNettyChannelBuilder
        .nameResolverFactory(new DnsNameResolverProvider)
        .executor((command: Runnable) => grpcExecutionContext.execute(command))
        .eventLoopGroup(eventLoopGroup)
        .channelType(classOf[NioSocketChannel])
        .usePlaintext()
        .build

    log.info(s"Building Blockchain Updates Extension gRPC client for server: ${wavesBlockchainClientSettings.blockchainUpdatesGrpc.target}")
    val blockchainUpdatesChannel: ManagedChannel =
      wavesBlockchainClientSettings.blockchainUpdatesGrpc.toNettyChannelBuilder
        .nameResolverFactory(new DnsNameResolverProvider)
        .executor((command: Runnable) => grpcExecutionContext.execute(command))
        .eventLoopGroup(eventLoopGroup)
        .channelType(classOf[NioSocketChannel])
        .usePlaintext()
        .build

    new CombinedWavesBlockchainClient(
      meClient = new MatcherExtensionCachingClient(
        new MatcherExtensionGrpcAsyncClient(eventLoopGroup, matcherExtensionChannel, monixScheduler)(grpcExecutionContext),
        wavesBlockchainClientSettings.defaultCachesExpiration
      )(grpcExecutionContext),
      bClient = new DefaultBlockchainUpdatesClient(eventLoopGroup, blockchainUpdatesChannel, monixScheduler)(grpcExecutionContext)
    )(grpcExecutionContext, monixScheduler)
  }

}
