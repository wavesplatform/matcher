package com.wavesplatform.it.matcher.api.http

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  private val negativeAssets = Table(
    ("Amount", "Price", "Http status", "Error code", "Message"),
    ("incorrect", "WAVES", 404, 11534345, "The asset incorrect not found"),
    ("WAVES", "incorrect", 404, 9440771, "The WAVES-incorrect asset pair should be reversed")
  )

  private val negativeDepth = Table(
    ("Value", "Title"),
    ("2147483648", "More than Int.Max"),
    ("100.0", "Double"),
    ("incorrect", "Alphabetical")
  )

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx)
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}?depth={depth} " - {

    "should return json with empty lists of ask and bids when the orderbook is empty" in {
      val r = validate200Json(dex1.rawApi.getOrderBook(wavesUsdPair))

      r.pair should be (wavesUsdPair)
      r.asks should have size 0
      r.bids should have size 0
    }

    "should return correct number of asks and bids" in {
      val ask = mkOrder(alice, wavesUsdPair, SELL, 10.waves, 5.usd)

      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 1.usd))
      placeAndAwaitAtDex(ask)
      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 3.waves, 4.usd))

      val r = validate200Json(dex1.rawApi.getOrderBook(wavesUsdPair))

      r.pair should be (wavesUsdPair)
      r.asks should have size 1
      r.bids should have size 2

      cancelAndAwait(alice, ask)

      validate200Json(dex1.rawApi.getOrderBook(wavesUsdPair)).asks should have size 0
    }

    "should return exception when price is not a correct base58 string" in {
      validate404Exception(dex1.rawApi.getOrderBook("WAVES", "null"))
    }

    forAll(negativeDepth) { (v: String, t: String) =>
      s"for depth = $v ($t) should return exception" in { // TODO fix, endpoint should return something like 400/Bad Request
        validate404Exception(dex1.rawApi.getOrderBook(wavesUsdPair, v))
      }
    }

    forAll(negativeAssets) { (a: String, p: String, c: Int, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.getOrderBook(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }
  }
}
