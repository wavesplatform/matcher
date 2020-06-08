package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.account.PublicKey
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
      |  "matcherPublicKey" : "2eEUvypDSivnzPiLrbYEW39SM8yMZ1aq4eJuiKfs4sEY",
      |  "matcherVersion" : "2.1.3.3",
      |  "priceAssets" : [ "WAVES", "4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8" ],
      |  "orderFee" : {
      |    "fixed" : {
      |      "assetId" : "4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8",
      |      "minFee" : 300000
      |    }
      |  },
      |  "orderVersions" : [ 1, 2, 3 ],
      |  "networkByte" : 83
      |}""".stripMargin

  private val issuedAsset = IssuedAsset(Base58.decode("4LHHvYGNKJUg5hj65aGD5vgScvCBmLpdRFtjokvCjSL8"))

  private val matcherPublicSettings =
    ApiMatcherPublicSettings(
      matcherPublicKey = PublicKey.fromBase58String("2eEUvypDSivnzPiLrbYEW39SM8yMZ1aq4eJuiKfs4sEY").right.get,
      matcherVersion = "2.1.3.3",
      priceAssets = Seq(Waves, issuedAsset),
      orderFee = ApiOrderFeeMode.FeeModeFixed(
        assetId = issuedAsset,
        minFee = 300000
      ),
      orderVersions = Seq(1, 2, 3),
      networkByte = 83
    )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[ApiMatcherPublicSettings] should matchTo(matcherPublicSettings)
    }

    "serialization" in {
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
