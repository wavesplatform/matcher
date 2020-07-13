package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.{WavesBlockchainCachingClient, WavesBlockchainClient, WavesBlockchainGrpcAsyncClient}
import com.wavesplatform.dex.grpc.integration.settings.WavesBlockchainClientSettings
import io.grpc.ManagedChannel
import io.grpc.internal.DnsNameResolverProvider
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel
import monix.execution.Scheduler

import scala.concurrent.{ExecutionContext, Future}

object WavesBlockchainClientBuilder extends ScorexLogging {

  def async(wavesBlockchainClientSettings: WavesBlockchainClientSettings,
            monixScheduler: Scheduler,
            grpcExecutionContext: ExecutionContext): WavesBlockchainClient[Future] = {

    log.info(s"Building gRPC client for server: ${wavesBlockchainClientSettings.grpc.target}")

    val eventLoopGroup = new NioEventLoopGroup

    val channel: ManagedChannel =
      wavesBlockchainClientSettings.grpc.toNettyChannelBuilder
        .nameResolverFactory(new DnsNameResolverProvider)
        .executor((command: Runnable) => grpcExecutionContext.execute(command))
        .eventLoopGroup(eventLoopGroup)
        .channelType(classOf[NioSocketChannel])
        .usePlaintext()
        .build

    new WavesBlockchainCachingClient(
      new WavesBlockchainGrpcAsyncClient(eventLoopGroup, channel, monixScheduler)(grpcExecutionContext),
      wavesBlockchainClientSettings.defaultCachesExpiration
    )(grpcExecutionContext)
  }
}
