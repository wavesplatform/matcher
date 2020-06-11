package com.wavesplatform.dex.api.http.entities

import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.model.LevelAgg
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class HttpV0OrderBookSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val json = """{
                       |  "timestamp" : 0,
                       |  "pair" : {
                       |    "amountAsset" : "LWbazpyvj625QB6EMC1vQoMkrvn2DdKjYbuuEw2T2UF",
                       |    "priceAsset" : "WAVES"
                       |  },
                       |  "bids" : [ {
                       |    "amount" : 10000000000000,
                       |    "price" : 41
                       |  }, {
                       |    "amount" : 2500000000000,
                       |    "price" : 40
                       |  }, {
                       |    "amount" : 300000000000000,
                       |    "price" : 1
                       |  } ],
                       |  "asks" : [ {
                       |    "amount" : 50000000000,
                       |    "price" : 50
                       |  }, {
                       |    "amount" : 2500000000000,
                       |    "price" : 51
                       |  } ]
                       |}""".stripMargin

  private val assetPair = AssetPair(Asset.fromString("LWbazpyvj625QB6EMC1vQoMkrvn2DdKjYbuuEw2T2UF").get, Waves)
  private val bids      = List(LevelAgg(10000000000000L, 41), LevelAgg(2500000000000L, 40), LevelAgg(300000000000000L, 1))
  private val asks      = List(LevelAgg(50000000000L, 50), LevelAgg(2500000000000L, 51))

  private val orderBookV0     = HttpV0OrderBook(0, assetPair, bids.map(HttpV0LevelAgg.fromLevelAgg), asks.map(HttpV0LevelAgg.fromLevelAgg))
  private val orderBookResult = HttpOrderBook(0, assetPair, bids, asks)

  "backward JSON compatibility" - {
    "deserialization" in {
      Json.parse(json).as[HttpV0OrderBook] should matchTo(orderBookV0)
    }

    "serialization" in {
      Json.prettyPrint(Json.parse(HttpOrderBook toJson orderBookResult)) should matchTo(json)
    }
  }
}
