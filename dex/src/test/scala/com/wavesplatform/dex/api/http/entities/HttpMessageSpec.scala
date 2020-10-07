package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpMessageSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json = """{"message":"test text"}"""
  private val message = HttpMessage("test text")

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpMessage] should matchTo(message)
    }

    "serialization" in {
      Json.stringify(Json.toJson(message)) should matchTo(json)
    }
  }
}
