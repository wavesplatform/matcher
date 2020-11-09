package com.wavesplatform.it.matcher.api.http

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.{HttpOrderBookStatus, HttpOrderStatus}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.model.{LastTrade, LevelAgg}
import com.wavesplatform.dex.model.OrderStatus.Filled
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBookStatusSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  private val negativeAssets = Table(
    ("Amount", "Price", "Http status", "Error code", "Message"),
    ("incorrect", "WAVES", 404, 11534345, "The asset incorrect not found"),
    ("WAVES", "incorrect", 404, 9440771, "The WAVES-incorrect asset pair should be reversed")
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx)
    broadcastAndAwait(IssueUsdTx)
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
        status.ask.get should be(1.9.usd)
        status.askAmount.get should be(1.waves)
        status.bid.get should be(1.1.usd)
        status.bidAmount.get should be(10.waves)

//        //TODO: will be fixed in the morning, want to sleep ))
//        status.lastPrice.get should be(LastTrade(1.waves, 1.9.usd, BUY))
//        status.bestBid.get should be(LevelAgg(1.waves, 1.1.usd))
//        status.bestAsk.get should be(LevelAgg(1.waves, 1.9.usd))
//      }
//
//      withClue("- trade (best sell order fully filled)") {
//        placeAndAwaitAtNode(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 2.usd))
//        val status = validate200Json(dex1.rawApi.getOrderBookStatus(wavesUsdPair))
//        status.ask.get should be(1.9.usd)
//        status.askAmount.get should be(1.waves)
//        status.bid.get should be(1.1.usd)
//        status.bidAmount.get should be(1.waves)
//        status.lastPrice.get should be(LastTrade(1.9.usd, 1.waves, BUY))
//        status.bestBid.get should be(LevelAgg(1.waves, 1.1.usd))
//        status.bestAsk.get should be(LevelAgg(1.waves, 1.9.usd))
//      }
    }

    "should return exception when amount is not a correct base58 string" in { // TODO: ? Create task for change it to matcherError?
      validate404Exception(dex1.rawApi.getOrderBookStatus("null", "WAVES"))
    }

    "should return exception when price is not a correct base58 string" in {
      validate404Exception(dex1.rawApi.getOrderBookStatus("WAVES", "null"))
    }

    forAll(negativeAssets) { (a: String, p: String, c: Int, e: Int, m: String) =>
      s"for $a/$p should return (HTTP-$c; [$e: $m]) " in {
        validateMatcherError(dex1.rawApi.getOrderBookStatus(AssetPair.createAssetPair(a, p).get), c, e, m)
      }
    }
  }
}
