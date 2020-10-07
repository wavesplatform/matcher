package com.wavesplatform.it.sync.api

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase

class GetSettingsUtilsTestSuite extends MatcherSuiteBase {

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  seed = "test"
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |}
       """.stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET matcher/debug/config should return config without restricted key-parts (e.g. seed)" in {
    dex1.api.config.toString should not contain "seed"
  }
}
