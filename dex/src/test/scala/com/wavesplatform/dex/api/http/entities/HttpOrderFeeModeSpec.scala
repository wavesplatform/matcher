package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.api.http.entities.HttpOrderFeeMode.{FeeModeComposite, FeeModeDynamic, FeeModeFixed, FeeModePercent, HttpDiscount}
import com.wavesplatform.dex.domain.asset.Asset.IssuedAsset
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.settings.AssetType
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{OptionValues, TryValues}
import play.api.libs.json.Json

class HttpOrderFeeModeSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits with TryValues with OptionValues {

  private val assetPair1 = AssetPair.createAssetPair(
    "DWgwcZTMhSvnyYCoWLRUXXSH1RSkzThXLJhww9gwkqdn",
    "25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT"
  ).success.value

  private val assetPair2 = AssetPair.createAssetPair(
    "FWgwcZTMhSvnyYCoWLRUXXSH1RSkzThXLJhww9gwkqdn",
    "25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT"
  ).success.value

  private val fixedModeJson: String = """{
                                        |  "fixed" : {
                                        |    "assetId" : "6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf",
                                        |    "minFee" : 1
                                        |  }
                                        |}""".stripMargin

  private val dynamicModeJson: String = """{
                                          |  "dynamic" : {
                                          |    "baseFee" : 600000
                                          |  }
                                          |}""".stripMargin

  private val percentModeJson: String = """{
                                          |  "percent" : {
                                          |    "type" : "price",
                                          |    "minFee" : 0.14,
                                          |    "minFeeInWaves" : 300000
                                          |  }
                                          |}""".stripMargin

  private val compositeModeJson: String =
    """{
      |  "composite" : {
      |    "default" : {
      |      "dynamic" : {
      |        "baseFee" : 600000
      |      }
      |    },
      |    "custom" : {
      |      "DWgwcZTMhSvnyYCoWLRUXXSH1RSkzThXLJhww9gwkqdn-25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT" : {
      |        "fixed" : {
      |          "assetId" : "6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf",
      |          "minFee" : 1
      |        }
      |      },
      |      "FWgwcZTMhSvnyYCoWLRUXXSH1RSkzThXLJhww9gwkqdn-25FEqEjRkqK6yCkiT7Lz6SAYz7gUFCtxfCChnrVFD5AT" : {
      |        "percent" : {
      |          "type" : "price",
      |          "minFee" : 0.14,
      |          "minFeeInWaves" : 300000
      |        }
      |      }
      |    },
      |    "discount" : {
      |      "assetId" : "6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf",
      |      "value" : 2
      |    }
      |  }
      |}""".stripMargin

  private val percentMode: HttpOrderFeeMode = FeeModePercent(AssetType.Price, 0.14, 300000)

  private val dynamicMode: HttpOrderFeeMode = FeeModeDynamic(600000)

  private val fixedMode: HttpOrderFeeMode = FeeModeFixed(IssuedAsset(Base58.decode("6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf")), 1)

  private val compositeMode: HttpOrderFeeMode = FeeModeComposite(
    dynamicMode,
    Map(
      assetPair1 -> fixedMode,
      assetPair2 -> percentMode
    ),
    Some(HttpDiscount(IssuedAsset(Base58.decode("6suw3ZHbyk6jrM19n7Pvaih3zSPsAt3gKcY8AZPxQYQf")), 2))
  )

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

    "Composite" - {
      "backward JSON compatibility" - {
        "deserialization" in {
          Json.parse(compositeModeJson).as[HttpOrderFeeMode] should matchTo(compositeMode)
        }

        "serialization" in {
          Json.prettyPrint(Json.toJson(compositeMode)) should matchTo(compositeModeJson)
        }
      }
    }
  }
}
