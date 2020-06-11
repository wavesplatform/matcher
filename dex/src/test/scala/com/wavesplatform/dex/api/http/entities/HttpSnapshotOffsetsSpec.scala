package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.bytes.codec.Base58
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpSnapshotOffsetsSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
      |  "2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh-WAVES" : 1
      |}""".stripMargin

  private val issuedAsset = IssuedAsset(Base58.decode("2gCPcEnoZa9LtZzZPFK9fJf7aWzvdBJUABayd1Zj5qFh"))

  private val snapshotOffsets = Map(
    AssetPair(issuedAsset, Waves) -> 1L
  )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpSnapshotOffsets] should matchTo(snapshotOffsets)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(snapshotOffsets)) should matchTo(json)
    }
  }
}
