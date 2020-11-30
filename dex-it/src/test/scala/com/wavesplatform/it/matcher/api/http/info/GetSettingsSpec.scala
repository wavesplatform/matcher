package com.wavesplatform.it.matcher.api.http.info

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class GetSettingsSpec extends MatcherSuiteBase with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/settings" - {

    "should return correct settings" in {
      val settings = validate200Json(dex1.rawApi.getMatcherSettings)

      settings.networkByte should be('Y'.toByte)
      settings.matcherPublicKey should be(matcher.publicKey)
      settings.priceAssets should have size 2
      settings.priceAssets should contain(usd)
    }
  }

}
