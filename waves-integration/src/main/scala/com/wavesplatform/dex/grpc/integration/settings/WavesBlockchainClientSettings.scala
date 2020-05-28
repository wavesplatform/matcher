package com.wavesplatform.dex.grpc.integration.settings

import scala.concurrent.duration.FiniteDuration

case class WavesBlockchainClientSettings(grpc: GrpcClientSettings, defaultCachesExpiration: FiniteDuration, balanceStreamBufferSize: Int)
