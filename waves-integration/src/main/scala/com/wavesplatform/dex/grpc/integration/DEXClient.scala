package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.grpc.integration.clients.async.WavesBalancesGrpcAsyncClient
import com.wavesplatform.dex.grpc.integration.clients.sync.WavesBlockchainGrpcSyncClient
import io.grpc._
import io.grpc.internal.DnsNameResolverProvider
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global

class DEXClient(target: String, val scheduler: Scheduler = global) {

  private val channel =
    ManagedChannelBuilder
      .forTarget(target)
      .nameResolverFactory(new DnsNameResolverProvider)
      .defaultLoadBalancingPolicy("round_robin")
      .usePlaintext()
      .build

  lazy val wavesBlockchainSyncClient = new WavesBlockchainGrpcSyncClient(channel)
  lazy val wavesBalancesAsyncClient  = new WavesBalancesGrpcAsyncClient(channel)(scheduler)
}
