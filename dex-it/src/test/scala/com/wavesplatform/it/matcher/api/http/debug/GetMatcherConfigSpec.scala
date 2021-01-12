package com.wavesplatform.it.matcher.api.http.debug

import com.wavesplatform.dex.settings.utils.ConfigOps.ConfigOps
import com.wavesplatform.it.matcher.api.http.HttpApiSuiteBase

class GetMatcherConfigSpec extends HttpApiSuiteBase {

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

  "GET /matcher/debug/config" - {

    "should return correct config" in {
      val config = validate200Hocon(dex1.rawApi.getMatcherConfig).rendered

      Set("user", "pass", "seed", "private", "java", "sun", "api").foreach(config should not contain _)
    }

    shouldReturnErrorWithoutApiKeyHeader

    shouldReturnErrorWithIncorrectApiKeyValue
  }
}
