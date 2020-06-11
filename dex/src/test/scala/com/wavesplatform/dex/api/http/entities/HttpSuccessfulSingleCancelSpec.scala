package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpSuccessfulSingleCancelSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
      |  "orderId" : "CijYneWqeJwtYLQvP3T6nRNueTFSmB977ULUDBxPZJNH",
      |  "success" : true,
      |  "status" : "OrderCanceled"
      |}""".stripMargin

  private val message = HttpSuccessfulSingleCancel(orderId = ByteStr.decodeBase58("CijYneWqeJwtYLQvP3T6nRNueTFSmB977ULUDBxPZJNH").get)

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpSuccessfulSingleCancel] should matchTo(message)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(message)) should matchTo(json)
    }
  }
}
