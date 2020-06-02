package com.wavesplatform.dex.api

import cats.syntax.option._
import com.wavesplatform.dex.domain.account.PublicKey
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.market.MatcherActor.AssetInfo
import com.wavesplatform.dex.test.matchers.DiffMatcherWithImplicits
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

class ApiTradingMarketsSpec extends AnyFreeSpec with Matchers with DiffMatcherWithImplicits {

  private val marketDataWithMetaJson = """{
                                         |  "amountAsset" : "89M5b2chgRya9A5WnJx4irNuKzXExdEko2m8jmzdFMX",
                                         |  "amountAssetName" : "AmountAsset",
                                         |  "amountAssetInfo" : {
                                         |    "decimals" : 8
                                         |  },
                                         |  "priceAsset" : "Chn6MVRNBk7mHNBQg67Zhc81aigaoZsLwwNo9QiJ5bd",
                                         |  "priceAssetName" : "PriceAsset",
                                         |  "priceAssetInfo" : {
                                         |    "decimals" : 8
                                         |  },
                                         |  "created" : 1591105681300,
                                         |  "matchingRules" : {
                                         |    "tickSize" : "0.1"
                                         |  }
                                         |}""".stripMargin

  private val tradingMarketsJson = """{
                                     |  "matcherPublicKey" : "J6ghck2hA2GNJTHGSLSeuCjKuLDGz8i83NfCMFVoWhvf",
                                     |  "markets" : [ {
                                     |    "amountAsset" : "89M5b2chgRya9A5WnJx4irNuKzXExdEko2m8jmzdFMX",
                                     |    "amountAssetName" : "AmountAsset",
                                     |    "amountAssetInfo" : {
                                     |      "decimals" : 8
                                     |    },
                                     |    "priceAsset" : "Chn6MVRNBk7mHNBQg67Zhc81aigaoZsLwwNo9QiJ5bd",
                                     |    "priceAssetName" : "PriceAsset",
                                     |    "priceAssetInfo" : {
                                     |      "decimals" : 8
                                     |    },
                                     |    "created" : 1591105681300,
                                     |    "matchingRules" : {
                                     |      "tickSize" : "0.1"
                                     |    }
                                     |  } ]
                                     |}""".stripMargin

  private val marketData =
    ApiMarketDataWithMeta(
      amountAsset = Asset.fromString("89M5b2chgRya9A5WnJx4irNuKzXExdEko2m8jmzdFMX").get,
      amountAssetName = "AmountAsset",
      amountAssetInfo = AssetInfo(8).some,
      priceAsset = Asset.fromString("Chn6MVRNBk7mHNBQg67Zhc81aigaoZsLwwNo9QiJ5bd").get,
      priceAssetName = "PriceAsset",
      priceAssetInfo = AssetInfo(8).some,
      created = 1591105681300L,
      restrictions = None,
      matchingRules = ApiOrderBookInfo.MatchingRuleSettings(0.1)
    )

  private val tradingMarkets =
    ApiTradingMarkets(
      PublicKey.fromBase58String("J6ghck2hA2GNJTHGSLSeuCjKuLDGz8i83NfCMFVoWhvf").right.get,
      Seq(marketData)
    )

  "backward JSON compatibility" - {

    "Market data with meta" - {
      "deserialization" in {
        Json.parse(marketDataWithMetaJson).as[ApiMarketDataWithMeta] should matchTo(marketData)
      }

      "serialization" in {
        Json.prettyPrint(Json.toJson(marketData)) should matchTo(marketDataWithMetaJson)
      }
    }

    "Trading markets" - {
      "deserialization" in {
        Json.parse(tradingMarketsJson).as[ApiTradingMarkets] should matchTo(tradingMarkets)
      }

      "serialization" in {
        Json.prettyPrint(Json.toJson(tradingMarkets)) should matchTo(tradingMarketsJson)
      }
    }
  }
}
