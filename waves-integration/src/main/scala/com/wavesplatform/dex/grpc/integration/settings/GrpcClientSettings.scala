package com.wavesplatform.dex.grpc.integration.settings

import com.wavesplatform.dex.grpc.integration.settings.GrpcClientSettings.ChannelOptionsSettings
import io.grpc.netty.{InternalNettyChannelBuilder, NettyChannelBuilder}
import io.netty.channel.ChannelOption

import scala.concurrent.duration.FiniteDuration

case class GrpcClientSettings(
  target: String,
  maxHedgedAttempts: Int,
  maxRetryAttempts: Int,
  keepAliveWithoutCalls: Boolean,
  keepAliveTime: FiniteDuration,
  keepAliveTimeout: FiniteDuration,
  idleTimeout: FiniteDuration,
  channelOptions: ChannelOptionsSettings,
  noDataTimeout: FiniteDuration
) {

  def toNettyChannelBuilder: NettyChannelBuilder = {
    val r = NettyChannelBuilder
      .forTarget(target)
      .maxHedgedAttempts(maxHedgedAttempts)
      .maxRetryAttempts(maxRetryAttempts)
      .keepAliveWithoutCalls(keepAliveWithoutCalls)
      .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
      .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
      .maxInboundMessageSize(4 * 1024 * 1024 * 5) //5 times bigger than default
      .idleTimeout(idleTimeout.length, idleTimeout.unit)
      .withOption[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, channelOptions.connectTimeout.toMillis.toInt)
    InternalNettyChannelBuilder.setStatsEnabled(r, false)
    InternalNettyChannelBuilder.setTracingEnabled(r, false)
    r
  }

}

object GrpcClientSettings {
  case class ChannelOptionsSettings(connectTimeout: FiniteDuration)
}
