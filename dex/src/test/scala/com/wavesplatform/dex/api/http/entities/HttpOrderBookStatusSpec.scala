package com.wavesplatform.dex.api.http.entities

import cats.syntax.option._
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsBoolean, JsString, Json}

class HttpOrderBookStatusSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

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
    HttpOrderBookStatus(
      lastPrice = 1000L.some,
      lastAmount = 2000L.some,
      lastSide = OrderType.SELL.some,
      bid = 2222L.some,
      bidAmount = 1111L.some,
      ask = 4444L.some,
      askAmount = 3333L.some
    )

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpOrderBookStatus] should matchTo(marketStatus)
    }

    "serialization" in {
      val marketStatusJson = Json.toJsObject(marketStatus) + ("success" -> JsBoolean(true)) + ("status" -> JsString("SimpleResponse"))
      Json.prettyPrint(marketStatusJson) should matchTo(json)
    }
  }
}
