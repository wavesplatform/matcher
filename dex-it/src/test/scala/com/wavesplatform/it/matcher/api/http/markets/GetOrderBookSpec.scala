package com.wavesplatform.it.matcher.api.http.markets

import sttp.model.StatusCode
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{AssetNotFound, InvalidBase58String, InvalidDepth, OrderAssetPairReversed}
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
        StatusCode.BadRequest,
        InvalidBase58String.code,
        s"Provided value is not a correct base58 string, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    forAll(Table(
      ("Value", "Title", "Error message"),
      ("2147483648", "More than Int.Max", "Provided depth is not correct, reason: Depth value '2147483648' must be an Integer"),
      ("100.0", "Double", "Provided depth is not correct, reason: Depth value '100.0' must be an Integer"),
      ("incorrect", "Alphabetical", "Provided depth is not correct, reason: Depth value 'incorrect' must be an Integer"),
      ("-1", "Less than zero", "Provided depth is not correct, reason: Depth value '-1' must be non-negative")
    )) { (v: String, t: String, m: String) =>
      s"for depth = $v ($t) should return exception" in {
        validateMatcherError(
          dex1.rawApi.getOrderBook(wavesUsdPair, v),
          StatusCode.BadRequest,
          InvalidDepth.code,
          m
        )
      }
    }

    forAll(Table(
      ("Amount", "Price", "Http status", "Error code", "Message"),
      ("incorrect", "WAVES", StatusCode.NotFound, AssetNotFound.code, "The asset incorrect not found"),
      ("WAVES", "incorrect", StatusCode.BadRequest, OrderAssetPairReversed.code, "The WAVES-incorrect asset pair should be reversed")
    )) { (a: String, p: String, c: StatusCode, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.getOrderBook(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }
  }
}
