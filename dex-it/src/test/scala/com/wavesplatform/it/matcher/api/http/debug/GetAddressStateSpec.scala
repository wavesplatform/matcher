package com.wavesplatform.it.matcher.api.http.debug

import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetAddressStateSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  "GET /matcher/debug/address/{address}" - {
    "should return correct state" in {
      validate200Json(dex1.rawApi.getAddressState(alice))
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getAddressState(alice, Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getAddressState(alice, incorrectApiKeyHeader))
  }

}
