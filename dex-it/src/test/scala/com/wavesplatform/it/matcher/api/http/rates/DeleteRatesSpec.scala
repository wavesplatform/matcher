package com.wavesplatform.it.matcher.api.http.rates

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpRates
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.it.MatcherSuiteBase

class DeleteRatesSpec extends MatcherSuiteBase with RawHttpChecks {

  val defaultRates: HttpRates = Map(Waves -> 1d)

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "DELETE /matcher/settings/rates/{assetId}" - {

    "should delete rate by asset id" in {
      validate201Json(dex1.rawApi.upsertRate(usd, 0.01)).message should be(s"The rate 0.01 for the asset $UsdId added")
      validate200Json(dex1.rawApi.deleteRate(usd)).message should be(s"The rate for the asset $UsdId deleted, old value = 0.01")
    }

    "should return error when rate was not specified" in {
      validateMatcherError(dex1.rawApi.deleteRate(usd), StatusCodes.NotFound, 20971529, s"The rate for the asset $UsdId was not specified")
    }

    "should return an error for unexisted asset" in {
      validateMatcherError(dex1.rawApi.deleteRate("AAA", Map("X-API-Key" -> apiKey)), StatusCodes.NotFound, 11534345, "The asset AAA not found")
    }

    "should return an error when user try to update Waves rate" in {
      validateMatcherError(dex1.rawApi.deleteRate(Waves), StatusCodes.BadRequest, 20971531, "The rate for WAVES cannot be changed")
    }

    "should return an error without X-API-KEY header" in {
      validateAuthorizationError(dex1.rawApi.deleteRate(UsdId.toString))
    }

    "should return an error with incorrect X-API-KEY header value" in {
      validateAuthorizationError(dex1.rawApi.deleteRate(UsdId.toString, Map("X-API-KEY" -> "incorrect")))
    }

    //TODO: change after DEX-980
    "should return an error when assetId is not a correct base58 string" in {
      validate404Exception(dex1.rawApi.deleteRate("null", Map("X-API-Key" -> apiKey)))
    }
  }
}
