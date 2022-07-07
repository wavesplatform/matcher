package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.matcher.api.http.ApiKeyHeaderChecks

class DeleteOrderBookKafkaTestSuite extends MatcherSuiteBase with ApiKeyHeaderChecks with HasKafka {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "$UsdnId", "WAVES" ]
       |}""".stripMargin
  )

  override protected lazy val dexRunConfig: Config = dexKafkaConfig()

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    kafka.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx, IssueUsdnTx)
    dex1.start()
  }

  "DELETE /matcher/orderbook/{amountAsset}/{priceAsset}" - {

    "should remove order book" in {
      val order = mkOrder(alice, wavesUsdPair, SELL, 10.waves, 1.usd)
      placeAndAwaitAtDex(order)

      0 until 10 foreach { _ =>
        placeAndAwaitAtDex(mkOrder(bob, btcUsdnPair, SELL, 10.waves, 1.usdn))
      }
      dex1.api.saveSnapshots
      Thread.sleep(5000L)

      dex1.restartWithNewSuiteConfig(ConfigFactory.parseString(
        s"""waves.dex {
           |  price-assets = [ "$UsdId", "$UsdnId", "WAVES" ]
           |  blacklisted-assets  = [$UsdId]
           |}""".stripMargin
      ))

      validate202Json(dex1.rawApi.deleteOrderBookWithKey(wavesUsdPair)).message should be("Deleting order book")
    }
  }

}
