package com.wavesplatform.it.matcher.api.http.debug

import com.wavesplatform.dex.api.http.entities.HttpSystemStatus
import com.wavesplatform.dex.app.MatcherStatus
import com.wavesplatform.dex.grpc.integration.clients.combined.CombinedStream
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetSystemStatusSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  "GET /matcher/debug/status" - {
    "should return matcher status" in {
      eventually {
        val httpSystemStatus = validate200Json(dex1.rawApi.getSystemStatus)
        httpSystemStatus.blockchain shouldBe a[CombinedStream.Status.Working]
        httpSystemStatus.service shouldBe MatcherStatus.Working
      }
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getSystemStatus(Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getSystemStatus(incorrectApiKeyHeader))
  }

}
