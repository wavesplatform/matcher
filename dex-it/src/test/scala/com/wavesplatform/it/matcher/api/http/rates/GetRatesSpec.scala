package com.wavesplatform.it.matcher.api.http.rates

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpRates
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class GetRatesSpec extends MatcherSuiteBase with RawHttpChecks {

  val defaultRates: HttpRates = Map(Waves -> 1d)

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

  "GET /matcher/settings/rates" - {

    "should return default value when rates were not set up" in {
      validate200Json(dex1.rawApi.getRates) should be(defaultRates)
    }

    "should return actual rates after update" in {
      validate201Json(dex1.rawApi.upsertRate(usd, 0.01))
      validate200Json(dex1.rawApi.getRates) should be(defaultRates + (usd -> 0.01))
    }

  }
}
