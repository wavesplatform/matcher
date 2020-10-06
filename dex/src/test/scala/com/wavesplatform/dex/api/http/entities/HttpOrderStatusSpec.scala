package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpOrderStatusSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  "backward JSON compatibility and converting from OrderStatus" - {
    Map[OrderStatus, String](
      OrderStatus.Accepted -> """{"status":"Accepted"}""",
      OrderStatus.NotFound -> """{"status":"NotFound","message":"The limit order is not found"}""",
      OrderStatus.PartiallyFilled(200, 10) -> """{"status":"PartiallyFilled","filledAmount":200,"filledFee":10}""",
      OrderStatus.Filled(201, 11) -> """{"status":"Filled","filledAmount":201,"filledFee":11}""",
      OrderStatus.Cancelled(202, 12) -> """{"status":"Cancelled","filledAmount":202,"filledFee":12}"""
    ).foreach {
      case (status, json) =>
        val apiStatus = HttpOrderStatus.from(status)

        status.name - {
          "deserialization" in {
            Json.parse(json).as[HttpOrderStatus] should matchTo(apiStatus)
          }

          "serialization" in {
            Json.stringify(Json.toJson(apiStatus)) should matchTo(json)
          }
        }
    }
  }
}
