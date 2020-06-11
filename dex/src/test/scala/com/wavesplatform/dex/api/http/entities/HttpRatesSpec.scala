package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpRatesSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
      |  "WAVES" : 1,
      |  "2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh" : 3
      |}""".stripMargin

  private val issuedAsset = IssuedAsset(Base58.decode("2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh"))

  private val rates =
    Map(
      Waves       -> 1d,
      issuedAsset -> 3d
    )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpRates] should matchTo(rates)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(rates)) should matchTo(json)
    }
  }
}
