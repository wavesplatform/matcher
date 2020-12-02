package com.wavesplatform.it.matcher.api.http.debug

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase

class PostSaveSnapshotsSpec extends MatcherSuiteBase with RawHttpChecks {

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

  "POST /matcher/debug/saveSnapshots" - {
    "should return save snapshors" in {
      List(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd)
      ).foreach(placeAndAwaitAtDex(_))

      validate200Json(dex1.rawApi.saveSnapshots).message should be("Saving started")
      eventually {
        validate200Json(dex1.rawApi.getAllSnapshotOffsets) should have size (1)
      }
    }

    "should return an error without X-API-KEY" in {
      validateAuthorizationError(dex1.rawApi.saveSnapshots(Map("X-API-KEY" -> "incorrect")))
    }

    "should return an error with incorrect X-API-KEY" in {
      validateAuthorizationError(dex1.rawApi.saveSnapshots(Map.empty))
    }
  }

}
