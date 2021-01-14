package com.wavesplatform.it.matcher.api.http.markets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class GetOrderBooksSpec extends MatcherSuiteBase with TableDrivenPropertyChecks with RawHttpChecks {

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

  "GET /matcher/orderbook " - {

    "should return correct matcher public key" in {
      validate200Json(dex1.rawApi.getOrderBooks).matcherPublicKey should be(matcher.publicKey)
    }

    "should return empty markets when there is no orderbooks" in {
      validate200Json(dex1.rawApi.getOrderBooks).markets should have size 0
    }

    "should return correct data of current active markets" in {
      placeAndAwaitAtDex(mkOrder(alice, wavesUsdPair, BUY, 1.waves, 1.usd))
      placeAndAwaitAtDex(mkOrder(alice, wavesBtcPair, SELL, 1.btc, 1.waves))

      val markets = validate200Json(dex1.rawApi.getOrderBooks).markets.map(x =>  AssetPair(x.amountAsset, x.priceAsset))
      markets should have size 2
      markets should contain (wavesBtcPair)
      markets should contain (wavesUsdPair)
    }
  }
}
