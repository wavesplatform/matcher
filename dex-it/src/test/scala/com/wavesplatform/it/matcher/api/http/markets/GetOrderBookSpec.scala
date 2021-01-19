package com.wavesplatform.it.matcher.api.http.markets

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}?depth={depth} " - {

    "should return json with empty lists of ask and bids when the orderbook is empty" in {
      val r = validate200Json(dex1.rawApi.getOrderBook(wavesUsdPair))

      r.pair should be(wavesUsdPair)
      r.asks should have size 0
      r.bids should have size 0
    }

    "should return correct number of asks and bids" in {
      val ask = mkOrder(alice, wavesUsdPair, SELL, 10.waves, 5.usd)

      List(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 1.usd), ask, mkOrder(alice, wavesUsdPair, BUY, 3.waves, 4.usd)).foreach(placeAndAwaitAtDex(_))

      val r = validate200Json(dex1.rawApi.getOrderBook(wavesUsdPair))

      r.pair should be(wavesUsdPair)
      r.asks should have size 1
      r.bids should have size 2

      cancelAndAwait(alice, ask)

      validate200Json(dex1.rawApi.getOrderBook(wavesUsdPair)).asks should have size 0
    }

    "should return exception when price is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderBook("WAVES", "null"),
        StatusCodes.BadRequest,
        9437185,
        s"Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    forAll(Table(
      ("Value", "Title"),
      ("2147483648", "More than Int.Max"),
      ("100.0", "Double"),
      ("incorrect", "Alphabetical")
    )) { (v: String, t: String) =>
      s"for depth = $v ($t) should return exception" in { // TODO fix, endpoint should return something like 400/Bad Request
        validate404Exception(dex1.rawApi.getOrderBook(wavesUsdPair, v))
      }
    }

    forAll(Table(
      ("Amount", "Price", "Http status", "Error code", "Message"),
      ("incorrect", "WAVES", 404, 11534345, "The asset incorrect not found"),
      ("WAVES", "incorrect", 404, 9440771, "The WAVES-incorrect asset pair should be reversed")
    )) { (a: String, p: String, c: Int, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.getOrderBook(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }
  }
}
