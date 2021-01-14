package com.wavesplatform.it.matcher.api.http.debug

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetAllSnapshotOffsetsSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET /matcher/debug/allSnapshotOffsets" - {
    "should return all saved snapshot offsets" in {
      validate200Json(dex1.rawApi.getAllSnapshotOffsets) should have size 0

      List(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 2.usd),
        mkOrder(bob, wavesBtcPair, BUY, 10.waves, 2.btc),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 3.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 4.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 5.usd),
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 6.usd)
      ).foreach(placeAndAwaitAtDex(_))

      dex1.api.saveSnapshots

      eventually {
        validate200Json(dex1.rawApi.getAllSnapshotOffsets) should have size 2
        validate200Json(dex1.rawApi.getAllSnapshotOffsets) should be(Map(wavesBtcPair -> 5, wavesUsdPair -> 5))
      }
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getAllSnapshotOffsets(Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getAllSnapshotOffsets(incorrectApiKeyHeader))
  }

}
