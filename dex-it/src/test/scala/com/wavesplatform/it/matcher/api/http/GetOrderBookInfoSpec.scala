package com.wavesplatform.it.matcher.api.http

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpMatchingRules
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookInfoSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  private val negativeCases = Table(
    ("Amount", "Price", "Http status", "Error code", "Message"),
    ("incorrect", "WAVES", 404, 11534345, "The asset incorrect not found"),
    ("WAVES", "incorrect", 404, 9440771, "The WAVES-incorrect asset pair should be reversed")
  )

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/info " - {

    "should return exception when amount is not a correct base58 string" in { // TODO: ? Create task for change it to matcherError?
      validate404Exception(dex1.rawApi.orderBookInfo("null", "WAVES"))
    }

    "should return exception when price is not a correct base58 string" in {
      validate404Exception(dex1.rawApi.orderBookInfo("WAVES", "null"))
    }

    forAll(negativeCases) { (a: String, p: String, c: Int, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.orderBookInfo(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }

    "should return correct data" in {
      validate200Json(dex1.rawApi.orderBookInfo(wavesUsdPair)).matchingRules should be(HttpMatchingRules(0.01))
    }
  }
}
