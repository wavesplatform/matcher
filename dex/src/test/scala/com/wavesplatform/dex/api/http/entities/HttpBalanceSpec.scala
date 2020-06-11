package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpBalanceSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
      |  "WAVES" : 100,
      |  "2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh" : 300
      |}""".stripMargin

  private val issuedAsset = IssuedAsset(Base58.decode("2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh"))

  private val balance =
    Map(
      Waves       -> 100L,
      issuedAsset -> 300L
    )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpBalance] should matchTo(balance)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(balance)) should matchTo(json)
    }
  }
}
