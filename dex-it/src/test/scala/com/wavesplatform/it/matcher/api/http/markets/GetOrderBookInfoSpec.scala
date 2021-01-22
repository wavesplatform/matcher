package com.wavesplatform.it.matcher.api.http.markets

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.{HttpMatchingRules, HttpOrderRestrictions}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookInfoSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  val expectedOrderRestrictions = HttpOrderRestrictions(
    0.1, 0.1, 100000000, 0.001, 0.0001, 1000
  )

  override protected def dexInitialSuiteConfig: Config = {
    import expectedOrderRestrictions._

    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |    order-restrictions = {
         |   "WAVES-$UsdId": {
         |     min-amount  = $minAmount
         |     max-amount  = $maxAmount
         |     step-amount = $stepAmount
         |     min-price   = $minPrice
         |     max-price   = $maxPrice
         |     step-price  = $stepPrice
         |   }
         | }
         |}""".stripMargin
    )
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/info " - {

    "should return exception when amount is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderBookInfo("null", "WAVES"),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return exception when price is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderBookInfo("WAVES", "null"),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return correct matching rules" in {
      validate200Json(dex1.rawApi.getOrderBookInfo(wavesUsdPair)).matchingRules should be(HttpMatchingRules(0.01))
    }

    "should return correct restrictions when it is in config" in {
      validate200Json(dex1.rawApi.getOrderBookInfo(wavesUsdPair)).restrictions.get should be(expectedOrderRestrictions)
    }

    "shouldn't return the restrictions object if it isn't in config" in {
      validate200Json(dex1.rawApi.getOrderBookInfo(wavesBtcPair)).restrictions should be(empty)
    }

    forAll(Table(
      ("Amount", "Price", "Http status", "Error code", "Message"),
      ("incorrect", "WAVES", 404, 11534345, "The asset incorrect not found"),
      ("WAVES", "incorrect", 404, 9440771, "The WAVES-incorrect asset pair should be reversed")
    )) { (a: String, p: String, c: Int, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.getOrderBookInfo(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }
  }
}
