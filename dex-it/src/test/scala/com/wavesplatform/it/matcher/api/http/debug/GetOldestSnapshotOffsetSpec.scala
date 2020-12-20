package com.wavesplatform.it.matcher.api.http.debug

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class GetOldestSnapshotOffsetSpec extends MatcherSuiteBase with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/debug/oldestSnapshotOffset" - {
    "should return oldest offset" in {
      validate200Json(dex1.rawApi.getOldestSnapshotOffset) should be(-1)

      List(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
      ).foreach(placeAndAwaitAtDex(_))

      validate200Json(dex1.rawApi.getOldestSnapshotOffset) should be(-1)
      dex1.api.saveSnapshots
      validate200Json(dex1.rawApi.getOldestSnapshotOffset) should be(2)
    }

    "should return an error without X-API-KEY" in {
      validateAuthorizationError(dex1.rawApi.getOldestSnapshotOffset(Map("X-API-KEY" -> "incorrect")))
    }

    "should return an error with incorrect X-API-KEY" in {
      validateAuthorizationError(dex1.rawApi.getOldestSnapshotOffset(Map.empty))
    }
  }

}