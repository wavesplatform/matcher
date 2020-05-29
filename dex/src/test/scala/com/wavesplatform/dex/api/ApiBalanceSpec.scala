package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiBalanceSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
  "WAVES" : 100,
  "2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh" : 300
}"""

  private val issuedAsset = IssuedAsset(Base58.decode("2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh"))
  private val balance = ApiBalance(
    Map(
      Waves       -> 100,
      issuedAsset -> 300
    )
  )

  "backward JSON compatibility" - {
    "serialization" in {
      Json.parse(json).as[ApiBalance] should matchTo(balance)
    }

    "deserialization" in {
      Json.prettyPrint(Json.toJson(balance)) should matchTo(json)
    }
  }
}
