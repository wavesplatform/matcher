package com.wavesplatform.dex.settings

import net.ceedubs.ficus.readers.NameMapper

import scala.concurrent.duration.FiniteDuration

case class ExchangeTransactionBroadcastSettings(broadcastUntilConfirmed: Boolean, interval: FiniteDuration, maxPendingTime: FiniteDuration)

object ExchangeTransactionBroadcastSettings {

  implicit val chosenCase: NameMapper = MatcherSettings.chosenCase
}
