package com.wavesplatform.dex.grpc.integration

import java.util.concurrent.TimeUnit

import com.wavesplatform.dex.grpc.integration.clients.async.WavesBlockchainCachingClient
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainGrpcSyncClient
import com.wavesplatform.utils.ScorexLogging
import io.grpc._
import io.grpc.internal.DnsNameResolverProvider
import monix.execution.Scheduler

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class DEXClient(target: String, defaultCachesExpiration: FiniteDuration, val monixScheduler: Scheduler, val grpcExecutionContext: ExecutionContext)
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

  lazy val wavesBlockchainSyncClient  = new WavesBlockchainGrpcSyncClient(channel)
  lazy val wavesBlockchainAsyncClient = new WavesBlockchainCachingClient(channel, defaultCachesExpiration, monixScheduler)(grpcExecutionContext)
}
