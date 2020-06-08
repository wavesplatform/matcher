package com.wavesplatform.dex.api

import cats.syntax.option._
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsBoolean, JsString, Json}

class ApiMarketStatusSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json =
    """{
      |  "lastPrice" : 1000,
      |  "lastAmount" : 2000,
      |  "lastSide" : "sell",
      |  "bid" : 2222,
      |  "bidAmount" : 1111,
      |  "ask" : 4444,
      |  "askAmount" : 3333,
      |  "success" : true,
      |  "status" : "SimpleResponse"
      |}""".stripMargin

  private val marketStatus =
    ApiMarketStatus(
      lastTrade = ApiLastTrade(1000, 2000, OrderType.SELL).some,
      bestBid = ApiV0LevelAgg(1111, 2222).some,
      bestAsk = ApiV0LevelAgg(3333, 4444).some
    )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[ApiMarketStatus] should matchTo(marketStatus)
    }

    "serialization" in {
      val marketStatusJson = Json.toJsObject(marketStatus) + ("success" -> JsBoolean(true)) + ("status" -> JsString("SimpleResponse"))
      Json.prettyPrint(marketStatusJson) should matchTo(json)
    }
  }
}
