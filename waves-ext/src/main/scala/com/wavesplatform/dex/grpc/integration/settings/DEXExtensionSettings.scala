package com.wavesplatform.dex.grpc.integration.settings

import scala.concurrent.duration.FiniteDuration

// TODO remove balanceChangesBatchLinger parameter after 2.1.2 release
final case class DEXExtensionSettings(host: String, port: Int, balanceChangesBatchLinger: FiniteDuration)
