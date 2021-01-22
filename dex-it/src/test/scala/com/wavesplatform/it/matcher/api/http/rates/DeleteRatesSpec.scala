package com.wavesplatform.it.matcher.api.http.rates

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpRates
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class DeleteRatesSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

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

    "should return an error when assetId is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.deleteRate("null", Map("X-API-Key" -> apiKey)),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.deleteRate(UsdId.toString, Map.empty))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.deleteRate(UsdId.toString, incorrectApiKeyHeader))
  }
}
