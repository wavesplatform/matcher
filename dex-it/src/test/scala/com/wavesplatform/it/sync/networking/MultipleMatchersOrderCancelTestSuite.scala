package com.wavesplatform.it.sync.networking

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItExternalKafkaRequired

@DexItExternalKafkaRequired
class MultipleMatchersOrderCancelTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  protected lazy val dex2: DexContainer = createDex("dex-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueEthTx)
    dex1.start()
    dex2.start()
  }

  /**
   *  Assumptions:
   *    1. DEX-1 is a master, DEX-2 is a slave;
   *    2. Latency in direction Kafka -> DEX-1 is too high and is ok in direction Kafka -> DEX-2, or master DEX is much more busy than slave one;
   *    3. DEX-1 and DEX-2 are connected to the same Node.
   *
   *  In this case orders on DEX-1 might be cancelled due to balance changing on Node (which were caused by exchange transactions from DEX-2)
   */
  "Tricky case when DEX-1 is slower than DEX-2 and it leads to order cancelling on DEX-1" in {

    val acc1 = mkAccountWithBalance(15.015.waves -> Waves)
    val acc2 = mkAccountWithBalance(0.015.waves -> Waves, 15.usd -> usd)
    val acc3 = mkAccountWithBalance(1.waves -> Waves, 10.eth -> eth) // Account for fake orders

    val ts = System.currentTimeMillis()
    val sellOrders = (1 to 5).map { amt =>
      mkOrderDP(acc1, wavesUsdPair, OrderType.SELL, amt.waves, amt, ts = ts + amt) // To cancel latest first
    }

    sellOrders.foreach(placeAndAwaitAtDex(_))

    // if DEX-1 will work with local queue, it won't receive buy orders placements and
    // will cancel remained orders due to balance changes
    // (which were caused by exchange transactions from DEX-2)

    dex1.api.saveSnapshots
    dex1.restartWithNewSuiteConfig(ConfigFactory.parseString(s"waves.dex.events-queue.type = local").withFallback(dexInitialSuiteConfig))
    // HACK: Because we switched the queue, we need to place 5 orders to move offset of queue.
    // If we don't do this, internal cancels will be ignored by order books.
    (1 to 5).foreach { _ =>
      dex1.api.place(mkOrderDP(acc3, ethWavesPair, OrderType.SELL, 1.eth, 1))
    }

    val submittedOrders = (1 to 3).map { amt =>
      mkOrderDP(acc2, wavesUsdPair, OrderType.BUY, amt.waves, amt)
    }
    submittedOrders.foreach(placeAndAwaitAtDex(_, Status.Filled, dex2))
    submittedOrders.foreach(waitForOrderAtNode(_, dex2.api))

    (0 to 2).foreach { i =>
      dex1.api.waitForOrderStatus(sellOrders(i), Status.Accepted)
    }

    // Matcher should prevent sell orders from cancelling!
    (3 to 4).foreach { i =>
      dex1.api.waitForOrderStatus(sellOrders(i), Status.Accepted)
    }
  }
}
