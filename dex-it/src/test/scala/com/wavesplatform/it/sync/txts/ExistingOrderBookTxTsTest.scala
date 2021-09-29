package com.wavesplatform.it.sync.txts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.MatcherSuiteBase

final class ExistingOrderBookTxTsTest extends MatcherSuiteBase {

  "Existing orderBook" - {
    "should create tx with same ts if feature start offset isn't reached" in test(1)

    "should create tx with different ts if feature start offset is reached" in test(2)
  }

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""|waves.dex {
          |  price-assets = [ "$UsdId", "WAVES" ]
          |  exchange-tx-ts-start-offset = 3
          |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  private def test(expectedTs: Int): Unit = {
    val orders = Seq(mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 5.usd), mkOrder(alice, wavesUsdPair, OrderType.BUY, 15.waves, 9.usd))
    orders.foreach(dex1.api.place)

    val bigOrderAmount = orders.map(_.amount).sum
    val bigOrderPrice = orders.map(_.price).min
    val bigOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, bigOrderAmount, bigOrderPrice)

    placeAndAwaitAtDex(bigOrder, Status.Filled)
    dex1.api.waitForTransactionsByOrder(bigOrder, 2).map(_.timestamp()).toSet.size shouldBe expectedTs
  }

}
