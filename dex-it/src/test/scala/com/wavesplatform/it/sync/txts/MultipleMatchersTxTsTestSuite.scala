package com.wavesplatform.it.sync.txts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.{Order, OrderType}
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItExternalKafkaRequired

import java.util.concurrent.ThreadLocalRandom

@DexItExternalKafkaRequired
final class MultipleMatchersTxTsTestSuite extends MatcherSuiteBase with HasKafka {

  private val topicName = s"test-${ThreadLocalRandom.current.nextInt(0, Int.MaxValue)}"

  "Multiple matchers orderBook" - {
    "should produce the same transactions" in {
      val co1 = placeOrderAndCounterOrder()

      dex2.disconnectFromNetwork()

      val co2 = placeOrderAndCounterOrder()
      val co3 = placeOrderAndCounterOrder()

      dex2.connectToNetwork()

      Thread.sleep(1000L)

      val aliceBalance1 = dex1.api.getReservedBalanceByPK(alice)
      val bobBalance1 = dex1.api.getReservedBalanceByPK(bob)

      val aliceBalance2 = dex2.api.getReservedBalanceByPK(alice)
      val bobBalance2 = dex2.api.getReservedBalanceByPK(bob)

      aliceBalance1 shouldBe aliceBalance2
      bobBalance1 shouldBe bobBalance2

      dex1.api.getLastOffset shouldBe dex2.api.getLastOffset
      compareTransactions(co1)
      compareTransactions(co2)
      compareTransactions(co3)
    }

  }

  private def compareTransactions(order: Order): Unit = {
    val tx1 = dex1.api.getTransactionsByOrderId(order.id()).head
    val tx2 = dex2.api.getTransactionsByOrderId(order.id()).head
    tx1.id() shouldBe tx2.id()
    tx1.timestamp() shouldBe tx2.timestamp()
  }

  private def placeOrderAndCounterOrder(): Order = {
    val order = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 5.usd)
    dex1.api.place(order)
    val counterOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 10.waves, 5.usd)
    placeAndAwaitAtDex(counterOrder, Status.Filled, isMarketOrder = true)
    dex1.api.waitForTransactionsByOrder(counterOrder, 1)
    counterOrder
  }

  override protected lazy val dexRunConfig = dexKafkaConfig(topicName)

  override protected def dexInitialSuiteConfig: Config =
    dexRunConfig.withFallback(ConfigFactory.parseString(
      s"""|waves.dex {
          |  price-assets = [ "$UsdId", "WAVES" ]
          |  exchange-tx-ts-start-offset = -1
          |}""".stripMargin
    ))

  protected lazy val dex2: DexContainer = createDex("dex-2", suiteInitialConfig = dexInitialSuiteConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    kafka.start()
    dex1.start()
    dex2.start()
  }

}
