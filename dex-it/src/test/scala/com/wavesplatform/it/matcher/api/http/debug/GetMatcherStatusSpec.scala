package com.wavesplatform.it.matcher.api.http.debug

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetMatcherStatusSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET /matcher/debug/status" - {
    "should return matcher status" in {
      val r = validate200Json(dex1.rawApi.getMatcherStatus)

      eventually {
        r("service") shouldBe "Working"
        r("blockchain") shouldBe "Working"
      }
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getMatcherStatus(Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getMatcherStatus(incorrectApiKeyHeader))
  }

}
