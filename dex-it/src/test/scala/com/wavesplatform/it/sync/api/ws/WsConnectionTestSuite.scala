package com.wavesplatform.it.sync.api.ws

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.WsSuiteBase

class WsConnectionTestSuite extends WsSuiteBase {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.price-assets = [ "$UsdId", "$BtcId", "WAVES", "$EthId" ]"""
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx, IssueEthTx)
    dex1.start()
  }

  "WS connection should" - {}
}
