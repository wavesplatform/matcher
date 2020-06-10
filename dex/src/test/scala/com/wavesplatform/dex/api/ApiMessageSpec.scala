package com.wavesplatform.dex.api

import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiMessageSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json    = """{"message":"test text"}"""
  private val message = ApiMessage("test text")

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[ApiMessage] should matchTo(message)
    }

    "serialization" in {
      Json.stringify(Json.toJson(message)) should matchTo(json)
    }
  }
}
