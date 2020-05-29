package com.wavesplatform.dex.api

import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiOffsetSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json   = """0"""
  private val offset = ApiOffset(0)

  "backward JSON compatibility" - {
    "serialization" in {
      Json.parse(json).as[ApiOffset] should matchTo(offset)
    }

    "deserialization" in {
      Json.toJson(offset).toString should matchTo(json)
    }
  }
}
