package com.wavesplatform.dex.grpc.integration

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.domain.utils.ScorexLogging
import com.wavesplatform.dex.grpc.integration.clients.{WavesBlockchainCachingClient, WavesBlockchainGrpcAsyncClient}
import io.grpc._
import io.grpc.internal.DnsNameResolverProvider
import monix.execution.Scheduler

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class DEXClient(target: String, defaultCachesExpiration: FiniteDuration)(implicit
                                                                         val monixScheduler: Scheduler,
                                                                         val grpcExecutionContext: ExecutionContext)
    extends ScorexLogging {

  log.info(s"NODE gRPC server: $target")

  private val channel =
    ManagedChannelBuilder
      .forTarget(target)
      .maxHedgedAttempts(10)
      .maxRetryAttempts(20)
      .keepAliveWithoutCalls(true)
      .keepAliveTime(2, TimeUnit.SECONDS)
      .keepAliveTimeout(5, TimeUnit.SECONDS)
      .nameResolverFactory(new DnsNameResolverProvider)
      .defaultLoadBalancingPolicy("pick_first")
      .usePlaintext()
      .build

  lazy val wavesBlockchainAsyncClient = new WavesBlockchainCachingClient(new WavesBlockchainGrpcAsyncClient(channel), defaultCachesExpiration)
}
