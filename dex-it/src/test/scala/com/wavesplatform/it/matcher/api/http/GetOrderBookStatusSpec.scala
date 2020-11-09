package com.wavesplatform.it.matcher.api.http

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
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
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "GET /matcher/orderbook/{amountAsset}/{priceAsset}/status " - {

    "debug " in {

      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 5.waves, 1.usd))
      val r = dex1.rawApi.getOrderBookStatus(wavesUsdPair)
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

//    "is returned even there is no such order book" in {
//      dex1.rawApi.getMarketStatus(bobNotTradedWavesPair).unsafeGet.leftSideValue should matchTo(HttpMarketStatus(None, None, None, None, None, None, None))
//    }
//
//    "should update after trade" in {
//      val ask = 5.waves
//      val askAmount = 5000000
//
//      val bid = 10.waves
//      val bidAmount = 10000000
//
//      dex1.api.place(mkOrder(bob, bob2WavesPair, SELL, askAmount, ask))
//
//      val resp1 = dex1.api.getMarketStatus(bob2WavesPair)
//      resp1.lastTrade shouldBe None
//      resp1.bestBid shouldBe None
//      resp1.bestAsk should matchTo {
//        Option(LevelAgg(askAmount, ask))
//      }
//
//      placeAndAwaitAtDex(mkOrder(alice, bob2WavesPair, BUY, bidAmount, bid))
//
//      val resp2 = dex1.api.getMarketStatus(bob2WavesPair)
//      resp2.lastTrade should matchTo {
//        Option(LastTrade(ask, askAmount, BUY))
//      }
//      resp2.bestBid should matchTo {
//        Option(LevelAgg(bidAmount - askAmount, bid))
//      }
//      resp2.bestAsk shouldBe None
//    }
  }
}
