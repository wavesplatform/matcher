package com.wavesplatform.it.matcher.api.http

import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class GetMatcherPublicKeySpec extends MatcherSuiteBase with RawHttpChecks {

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    dex1.start()
  }

  "GET /matcher" - {
    "should return correct public key of matcher" in {
      validate200Json(dex1.rawApi.getMatcherPublicKey) should be(matcher.publicKey)
    }
  }
}
