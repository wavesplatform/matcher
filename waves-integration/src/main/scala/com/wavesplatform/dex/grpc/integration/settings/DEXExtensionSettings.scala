package com.wavesplatform.dex.grpc.integration.settings

import scala.concurrent.duration.FiniteDuration

final case class DEXExtensionSettings(host: String, port: Int, balanceChangesBatchLinger: FiniteDuration)
