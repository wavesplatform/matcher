package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.api.http.entities.HttpOrderFeeMode.{FeeModeDynamic, FeeModeFixed, FeeModePercent}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpOrderFeeModeSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val fixedModeJson: String = """{
                                          |  "fixed" : {
                                          |    "assetId" : "6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf",
                                          |    "minFee" : 1
                                          |  }
                                          |}""".stripMargin

  private val fixedMode: HttpOrderFeeMode = FeeModeFixed(IssuedAsset(Base58.decode("6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf")), 1)

  private val dynamicModeJson: String = """{
                                          |  "dynamic" : {
                                          |    "baseFee" : 600000,
                                          |    "rates" : {
                                          |      "WAVES" : 1
                                          |    }
                                          |  }
                                          |}""".stripMargin

  private val percentModeJson: String = """{
                                          |  "percent" : {
                                          |    "type" : "price",
                                          |    "minFee" : 0.14
                                          |  }
                                          |}""".stripMargin

  private val percentMode: HttpOrderFeeMode = FeeModePercent(AssetType.PRICE, 0.14)

  private val dynamicMode: HttpOrderFeeMode = FeeModeDynamic(600000, Map(Waves -> 1))

  "ApiOrderFeeMode" - {
    "Dynamic" - {
      "backward JSON compatibility" - {
        "deserialization" in {
          Json.parse(dynamicModeJson).as[HttpOrderFeeMode] should matchTo(dynamicMode)
        }
        "serialization" in {
          Json.prettyPrint(Json.toJson(dynamicMode)) should matchTo(dynamicModeJson)
        }
      }
    }

    "Fixed" - {
      "backward JSON compatibility" - {
        "deserialization" in {
          Json.parse(fixedModeJson).as[HttpOrderFeeMode] should matchTo(fixedMode)
        }
        "serialization" in {
          Json.prettyPrint(Json.toJson(fixedMode)) should matchTo(fixedModeJson)
        }
      }
    }

    "Percent" - {
      "backward JSON compatibility" - {
        "deserialization" in {
          Json.parse(percentModeJson).as[HttpOrderFeeMode] should matchTo(percentMode)
        }
        "serialization" in {
          Json.prettyPrint(Json.toJson(percentMode)) should matchTo(percentModeJson)
        }
      }
    }
  }
}
