package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.MatcherState

class MarketStatusRecoveryTestSuite extends MatcherRecoveryTestSuite {

  // To create a snapshot for each event at least for one order book
  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString("waves.dex.snapshots-interval = 2").withFallback(super.dexInitialSuiteConfig)

  override protected def cleanState(state: MatcherState): MatcherState = state.copy(snapshots = Map.empty)
}
