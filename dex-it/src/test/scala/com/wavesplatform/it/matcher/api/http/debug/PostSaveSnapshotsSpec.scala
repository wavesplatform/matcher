package com.wavesplatform.it.matcher.api.http.debug

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class PostSaveSnapshotsSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

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
        validate200Json(dex1.rawApi.getAllSnapshotOffsets) should have size 1
      }
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.saveSnapshots(Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.saveSnapshots(incorrectApiKeyHeader))
  }

}
