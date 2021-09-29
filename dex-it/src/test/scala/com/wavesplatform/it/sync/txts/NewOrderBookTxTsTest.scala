package com.wavesplatform.it.sync.txts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.MatcherSuiteBase

final class NewOrderBookTxTsTest extends MatcherSuiteBase {

  "New orderBook" - {
    "should create tx with different ts if feature turned on from start" in {
      val orders = Seq(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 5.usd),
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 15.waves, 9.usd),
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 17.waves, 11.usd)
      )

      orders.foreach(dex1.api.place)

      val bigOrderAmount = orders.map(_.amount).sum
      val bigOrderPrice = orders.map(_.price).min
      val bigOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, bigOrderAmount, bigOrderPrice)

      placeAndAwaitAtDex(bigOrder, Status.Filled, isMarketOrder = true)
      dex1.api.waitForTransactionsByOrder(bigOrder, 3).map(_.timestamp()).toSet.size shouldBe 3
    }
  }

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""|waves.dex {
          |  price-assets = [ "$UsdId", "WAVES" ]
          |  exchange-tx-ts-start-offset = -1
          |}""".stripMargin // turning on this feature at the first command
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

}
