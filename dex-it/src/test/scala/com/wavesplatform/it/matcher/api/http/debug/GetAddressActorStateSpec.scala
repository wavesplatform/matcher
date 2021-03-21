package com.wavesplatform.it.matcher.api.http.debug

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class GetAddressActorStateSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

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

  "GET /matcher/debug/address/{address}" - {
    "should return correct state" in {
      validate200Json(dex1.rawApi.getAddressActorState(toAddress(alice).stringRepr))
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.getAddressActorState(toAddress(alice).stringRepr, Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.getAddressActorState(toAddress(alice).stringRepr, incorrectApiKeyHeader))
  }

}
