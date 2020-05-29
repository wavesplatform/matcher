package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiRatesSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
  "WAVES" : 1,
  "2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh" : 3
}"""

  private val issuedAsset = IssuedAsset(Base58.decode("2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh"))
  private val rates = ApiRates(
    Map(
      Waves       -> 1,
      issuedAsset -> 3
    )
  )

  "backward JSON compatibility" - {
    "serialization" in {
      Json.parse(json).as[ApiRates] should matchTo(rates)
    }

    "deserialization" in {
      Json.prettyPrint(Json.toJson(rates)) should matchTo(json)
    }
  }
}
