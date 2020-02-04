package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiMatcherPublicSettingsSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  // TODO "WAVES" vs null

  private val json =
    """{
  "priceAssets" : [ null, "4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8" ],
  "orderFee" : {
    "fixed" : {
      "assetId" : "4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8",
      "minFee" : 300000
    }
  },
  "orderVersions" : [ 1, 2, 3 ]
}"""

  private val issuedAsset = IssuedAsset(Base58.decode("4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8"))
  private val matcherPublicSettings = ApiMatcherPublicSettings(
    priceAssets = Seq(Waves, issuedAsset),
    orderFee = ApiMatcherPublicSettings.ApiOrderFeeSettings.Fixed(
      assetId = issuedAsset,
      minFee = 300000
    ),
    orderVersions = Seq(1, 2, 3)
  )

  "backward JSON compatibility" - {
    "serialization" in {
      Json.parse(json).as[ApiMatcherPublicSettings] should matchTo(matcherPublicSettings)
    }

    "deserialization" in {
      Json.prettyPrint(Json.toJson(matcherPublicSettings)) should matchTo(json)
    }

    "OrderFeePublicSettings" - {
      "Dynamic" - {
        "serialization" ignore {}
        "deserialization" ignore {}
      }

      "Fixed" - {
        "serialization" ignore {}
        "deserialization" ignore {}
      }

      "Percent" - {
        "serialization" ignore {}
        "deserialization" ignore {}
      }
    }
  }
}
