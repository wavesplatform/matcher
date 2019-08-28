package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.grpc.integration.clients.BalancesServiceClient
import io.grpc._
import io.grpc.internal.DnsNameResolverProvider

class DEXClient(target: String) {

  private val channel =
    ManagedChannelBuilder
      .forTarget(target)
      .nameResolverFactory(new DnsNameResolverProvider)
      .defaultLoadBalancingPolicy("round_robin")
      .usePlaintext()
      .build

  lazy val balancesServiceClient = new BalancesServiceClient(channel)
}
