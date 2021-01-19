package com.wavesplatform.it.matcher.api.http.rates

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpRates
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.it.docker.apiKey
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class UpdateRatesByAssetIdSpec extends MatcherSuiteBase with ApiKeyHeaderChecks {

  val defaultRates: HttpRates = Map(Waves -> 1d)

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  "PUT /matcher/settings/rates/{assetId}" - {

    "should update rate by asset id" in {

      withClue(" - asset doesn't have a rate") {
        validate201Json(dex1.rawApi.upsertRate(usd, 0.01)).message should be(s"The rate 0.01 for the asset $UsdId added")
      }

      withClue(" - asset already have a rate") {
        validate200Json(dex1.rawApi.upsertRate(usd, 0.02)).message should be(
          s"The rate for the asset $UsdId updated, old value = 0.01, new value = 0.02"
        )
      }
    }

    "should return an error for incorrect rate values" in {
      validateMatcherError(dex1.rawApi.upsertRate(btc, -1), StatusCodes.BadRequest, 20971535, "Asset rate should be positive")
      validateMatcherError(dex1.rawApi.upsertRate(btc, 0), StatusCodes.BadRequest, 20971535, "Asset rate should be positive")
    }

    //TODO: DEX-985
    "should return error if  the rate value more than Double.max" ignore {
      validateMatcherError(dex1.rawApi.upsertRate(btc, "2.79769311348623157E308"), StatusCodes.BadRequest, -1, "Error")
    }

    "should return an error for unexisted asset" in {
      validateMatcherError(
        dex1.rawApi.upsertRate("AAA", 0.5, Map("X-API-Key" -> apiKey)),
        StatusCodes.NotFound,
        11534345,
        "The asset AAA not found"
      )
    }

    "should return an error when user try to update Waves rate" in {
      validateMatcherError(dex1.rawApi.upsertRate(Waves, 0.5), StatusCodes.BadRequest, 20971531, "The rate for WAVES cannot be changed")
    }

    "should return error exception when the amount asset is not correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.upsertRate("null", 0.1, Map("X-API-Key" -> apiKey)),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    shouldReturnErrorWithoutApiKeyHeader(dex1.rawApi.upsertRate(UsdId.toString, 0.5))

    shouldReturnErrorWithIncorrectApiKeyValue(dex1.rawApi.upsertRate(UsdId.toString, 0.5, incorrectApiKeyHeader))
  }
}
