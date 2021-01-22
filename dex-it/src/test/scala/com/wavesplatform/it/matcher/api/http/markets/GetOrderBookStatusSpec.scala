package com.wavesplatform.it.matcher.api.http.markets

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderBookStatus
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.model.{LastTrade, LevelAgg}
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookStatusSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

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

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/status " - {

    "should correctly update the status and data of orderbook " in {

      validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))

      withClue("- bid amount and price (one order)") {
        placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 5.waves, 1.usd))
        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))
        status.bid.get should be(1.usd)
        status.bidAmount.get should be(5.waves)
      }

      withClue("- bid amount and price (multiple orders)") {
        placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.1.usd))
        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))
        status.bid.get should be(1.1.usd)
        status.bidAmount.get should be(10.waves)
      }

      withClue("- ask amount and price (one order)") {
        placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, SELL, 2.waves, 1.9.usd))
        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))
        status.ask.get should be(1.9.usd)
        status.askAmount.get should be(2.waves)
      }

      withClue("- ask amount and price (multiple orders)") {
        placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, SELL, 4.waves, 2.usd))
        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))
        status.ask.get should be(1.9.usd)
        status.askAmount.get should be(2.waves)
      }

      withClue("- trade (best sell order partially filled)") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 2.usd))
        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))

        status should matchTo(HttpOrderBookStatus(
          Some(1.9.usd),
          Some(1.waves),
          Some(BUY),
          Some(1.1.usd),
          Some(10.waves),
          Some(1.9.usd),
          Some(1.waves)
        ))

        status.lastTrade.get should be(LastTrade(1.9.usd, 1.waves, BUY))
        status.bestBid.get should be(LevelAgg(10.waves, 1.1.usd))
        status.bestAsk.get should be(LevelAgg(1.waves, 1.9.usd))
      }

      withClue("- trade (best sell order fully filled)") {
        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 2.usd))
        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))

        status should matchTo(HttpOrderBookStatus(
          Some(1.9.usd),
          Some(1.waves),
          Some(BUY),
          Some(1.1.usd),
          Some(10.waves),
          Some(2.usd),
          Some(4.waves)
        ))

        status.lastTrade.get should be(LastTrade(1.9.usd, 1.waves, BUY))
        status.bestBid.get should be(LevelAgg(10.waves, 1.1.usd))
        status.bestAsk.get should be(LevelAgg(4.waves, 2.usd))
      }
    }

    "should return empty json for not traded order book" in {
      validate200Json(dex1.rawApi.getOrderBookStatus(wavesBtcPair)) should matchTo(HttpOrderBookStatus(None, None, None, None, None, None, None))
    }

    "should return exception when amount is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderBookStatus("null", "WAVES"),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    "should return exception when price is not a correct base58 string" in {
      validateMatcherError(
        dex1.rawApi.getOrderBookStatus("WAVES", "null"),
        StatusCodes.BadRequest,
        11534337,
        "The asset 'null' is wrong, reason: requirement failed: Wrong char 'l' in Base58 string 'null'"
      )
    }

    forAll(Table(
      ("Amount", "Price", "Http status", "Error code", "Message"),
      ("incorrect", "WAVES", 404, 11534345, "The asset incorrect not found"),
      ("WAVES", "incorrect", 404, 9440771, "The WAVES-incorrect asset pair should be reversed")
    )) { (a: String, p: String, c: Int, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.getOrderBookStatus(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }
  }
}
