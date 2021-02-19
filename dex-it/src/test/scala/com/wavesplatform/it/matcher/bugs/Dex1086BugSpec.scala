package com.wavesplatform.it.matcher.bugs

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.it.MatcherSuiteBase

// Broken auto cancel
class Dex1086BugSpec extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(s"""waves.dex {
  price-assets = [ "$UsdId", "WAVES" ]
  snapshots-interval = 1
}""")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "DEX-1086" in {
    log.info(s"bob Waves balance: ${wavesNode1.api.balance(bob, Waves)}")

    step("placing orders")
    dex1.api.place(mkOrderDP(alice, wavesUsdPair, OrderType.BUY, 100.waves, 15))

    val now = System.currentTimeMillis()
    val orders = (1 to 10).map { i =>
      mkOrderDP(bob, wavesUsdPair, OrderType.SELL, 10.waves, 15, ts = now + i)
    }

    orders.foreach(dex1.api.place(_))
    orders.foreach(waitForOrderAtNode(_))

    log.info(s"bob Waves balance: ${wavesNode1.api.balance(bob, Waves)}")

    step("saving snapshots and restart the matcher")
    dex1.api.saveSnapshots
    eventually {
      dex1.api.getOldestSnapshotOffset shouldBe 10L // 1 + 10 orders, the index starts from 0
    }
    dex1.restart()

    step("trying to place an order without enough funds")
    val bobOrder = mkOrderDP(bob, wavesUsdPair, OrderType.SELL, wavesNode1.api.balance(bob, Waves) - matcherFee, 15)
    placeAndAwaitAtDex(bobOrder)

    broadcastAndAwait(mkTransfer(bob, alice, 1.waves, Waves))
    dex1.api.waitForOrderStatus(bobOrder, Status.Cancelled)
  }

}
