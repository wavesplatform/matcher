package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiSuccessfulCancelSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
  "orderId" : "CijYneWqeJwtYLQvP3T6nRNueTFSmB977ULUDBxPZJNH",
  "success" : true,
  "status" : "OrderCanceled"
}"""

  private val message = ApiSuccessfulCancel(orderId = ByteStr.decodeBase58("CijYneWqeJwtYLQvP3T6nRNueTFSmB977ULUDBxPZJNH").get)

  "backward JSON compatibility" - {
    "serialization" in {
      Json.parse(json).as[ApiSuccessfulCancel] should matchTo(message)
    }

    "deserialization" in {
      Json.prettyPrint(Json.toJson(message)) should matchTo(json)
    }
  }
}
