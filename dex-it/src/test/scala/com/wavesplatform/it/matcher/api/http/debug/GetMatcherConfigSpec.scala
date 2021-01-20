package com.wavesplatform.it.matcher.api.http.debug

import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetMatcherConfigSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

  "GET /matcher/debug/config" - {

    "should return correct config" in {
      val config = validate200Hocon(dex1.rawApi.getMatcherConfig).rendered

      Set("user", "pass", "seed", "private", "java", "sun", "api").foreach(config should not contain _)
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getMatcherConfig(Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getMatcherConfig(incorrectApiKeyHeader))
  }
}
