package com.wavesplatform.it.matcher.api.http.debug

import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import com.wavesplatform.it.MatcherSuiteBase

class GetMatcherConfigSpec extends MatcherSuiteBase with RawHttpChecks {

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

  "GET /matcher/debug/config" - {

    "should return correct config" in {
      val config = validate200Hocon(dex1.rawApi.getMatcherConfig).rendered

      Set("user", "pass", "seed", "private", "java", "sun", "api").foreach(config should not contain _)
    }

    "should return correct error with incorrect x-api-key header" in {
      validateAuthorizationError(dex1.rawApi.getMatcherConfig(Map("X-API_KEY" -> "incorrect")))
    }

    "should return correct error without x-api-key header" in {
      validateAuthorizationError(dex1.rawApi.getMatcherConfig(Map.empty))
    }
  }

}
