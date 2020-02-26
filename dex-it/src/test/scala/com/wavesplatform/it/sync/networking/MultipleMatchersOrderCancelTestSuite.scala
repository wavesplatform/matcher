package com.wavesplatform.it.sync.networking

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItKafkaRequired
import org.scalatest

@DexItKafkaRequired
class MultipleMatchersOrderCancelTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  protected lazy val dex2: DexContainer = createDex("dex-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
    dex2.start()
  }

  private def createAccountWithBalance(balances: (Long, Asset)*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${ThreadLocalRandom.current().nextInt()}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) =>
        asset.fold { scalatest.Assertions.succeed } { issuedAsset =>
          assert(
            wavesNode1.api.assetBalance(alice, issuedAsset).balance >= balance,
            s"Alice doesn't have enough balance in ${issuedAsset.toString} to make a transfer"
          )
        }
        broadcastAndAwait { mkTransfer(alice, account, balance, asset, 0.003.waves) }
    }
    account
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

    val acc1 = createAccountWithBalance(15.015.waves -> Waves)
    val acc2 = createAccountWithBalance(0.015.waves  -> Waves, 15.usd -> usd)

    val sellOrders = (1 to 5).map { amt =>
      mkOrderDP(acc1, wavesUsdPair, OrderType.SELL, amt.waves, amt)
    }

    sellOrders.foreach { placeAndAwaitAtDex(_) }

    // if DEX-1 will work with local queue, it won't receive buy orders placements and
    // will cancel remained orders due to balance changes
    // (which were caused by exchange transactions from DEX-2)

    dex1.api.saveSnapshots
    dex1.restartWithNewSuiteConfig(ConfigFactory.parseString(s"waves.dex.events-queue.type = local").withFallback(dexInitialSuiteConfig))

    (1 to 3).foreach { amt =>
      val order = mkOrderDP(acc2, wavesUsdPair, OrderType.BUY, amt.waves, amt)
      dex2.api.place(order)
      dex2.api.waitForOrderStatus(order, OrderStatus.Filled)
    }

    // problem solution should prevent sell orders from cancelling!
    dex1.api.waitForOrderStatus(sellOrders(4), OrderStatus.Cancelled)
    dex1.api.waitForOrderStatus(sellOrders(3), OrderStatus.Cancelled)
  }
}
