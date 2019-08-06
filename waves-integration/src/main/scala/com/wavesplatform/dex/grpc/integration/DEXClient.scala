package com.wavesplatform.dex.grpc.integration

import com.wavesplatform.dex.grpc.integration.clients.BalanceServiceClient
import io.grpc._
import io.grpc.internal.DnsNameResolverProvider
import io.grpc.util.RoundRobinLoadBalancerFactory

class DEXClient(target: String) {

  private val channel =
    ManagedChannelBuilder
      .forTarget(target)
      .nameResolverFactory(new DnsNameResolverProvider)
      .loadBalancerFactory(RoundRobinLoadBalancerFactory.getInstance)
      .usePlaintext()
      .build

  lazy val balanceServiceClient = new BalanceServiceClient(channel)
}
