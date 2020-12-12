package com.wavesplatform.dex.grpc.integration.settings

import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedWavesBlockchainClient

import scala.concurrent.duration.FiniteDuration

case class WavesBlockchainClientSettings(
  grpc: GrpcClientSettings,
  blockchainUpdatesGrpc: GrpcClientSettings,
  defaultCachesExpiration: FiniteDuration,
  balanceStreamBufferSize: Int,
  combinedClientSettings: CombinedWavesBlockchainClient.Settings
)
