package com.wavesplatform.it.matcher.api.http.history

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair.toAddress
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.model.OrderStatus
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.http.toHttpOrderBookHistoryItem

class GetOrderHistoryByApiKeySpec extends MatcherSuiteBase with RawHttpChecks {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  "GET /matcher/orders/{address}" - {
    "should return all order history" in {

      val orders = Array(
        mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd),
        mkOrder(alice, wavesUsdPair, BUY, 11.waves, 2.usd),
        mkOrder(alice, wavesUsdPair, BUY, 12.waves, 3.usd)
      ).map { order =>
        placeAndAwaitAtDex(order)
        toHttpOrderBookHistoryItem(order, OrderStatus.Accepted)
      }

      val history = validate200Json(dex1.rawApi.getOrderHistoryByApiKey(toAddress(alice).stringRepr))

      history should have size orders.length
      orders.foreach(history should contain(_))
    }

  }

}
