package com.wavesplatform.it.matcher.api.http.debug

import com.wavesplatform.dex.statuses.{CombinedStreamStatus, MatcherStatus}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetMatcherStatusSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  "GET /matcher/debug/status" - {
    "should return matcher status" in {
      eventually {
        val httpSystemStatus = validate200Json(dex1.rawApi.getMatcherStatus)
        httpSystemStatus.blockchain shouldBe a[CombinedStreamStatus.Working]
        httpSystemStatus.service shouldBe MatcherStatus.Working
      }
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getMatcherStatus(Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getMatcherStatus(incorrectApiKeyHeader))
  }

}
