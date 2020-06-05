package com.wavesplatform.dex.api

import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiSuccessfulSingleCancelSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
      |  "orderId" : "CijYneWqeJwtYLQvP3T6nRNueTFSmB977ULUDBxPZJNH",
      |  "success" : true,
      |  "status" : "OrderCanceled"
      |}""".stripMargin

  private val message = ApiSuccessfulSingleCancel(orderId = ByteStr.decodeBase58("CijYneWqeJwtYLQvP3T6nRNueTFSmB977ULUDBxPZJNH").get)

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[ApiSuccessfulSingleCancel] should matchTo(message)
    }

    "serialization" in {
      Json.prettyPrint(Json.toJson(message)) should matchTo(json)
    }
  }
}
