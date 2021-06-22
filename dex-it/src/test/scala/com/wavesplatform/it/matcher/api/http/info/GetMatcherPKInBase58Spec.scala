package com.wavesplatform.it.matcher.api.http.info

import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class GetMatcherPKInBase58Spec extends MatcherSuiteBase with RawHttpChecks {

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "GET /matcher" - {
    "should return correct public key of matcher in base58" in {
      validate200Json(dex1.rawApi.getMatcherPKInBase58) should be(matcher.publicKey.toString)
    }
  }
}
