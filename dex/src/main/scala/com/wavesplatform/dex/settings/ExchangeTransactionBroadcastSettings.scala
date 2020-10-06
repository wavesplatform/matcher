package com.wavesplatform.dex.settings

import scala.concurrent.duration.FiniteDuration

case class ExchangeTransactionBroadcastSettings(broadcastUntilConfirmed: Boolean, interval: FiniteDuration, maxPendingTime: FiniteDuration)
