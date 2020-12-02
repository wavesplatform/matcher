package com.wavesplatform.dex.grpc.integration.settings

import scala.concurrent.duration.FiniteDuration

case class WavesBlockchainClientSettings(
  grpc: GrpcClientSettings,
  blockchainUpdatesGrpc: GrpcClientSettings,
  defaultCachesExpiration: FiniteDuration,
  balanceStreamBufferSize: Int
)
