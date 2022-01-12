package com.wavesplatform.it.sync.kafka.issues

import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.api.ws.converters.WsOrderConverter
import com.wavesplatform.dex.api.ws.entities.{WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.OrderStatusNames
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.it.api.websockets.HasWebSockets
import com.wavesplatform.dex.it.containers.HasKafka
import com.wavesplatform.dex.model.LimitOrder
import com.wavesplatform.it.WsSuiteBase

import java.util.concurrent.ThreadLocalRandom
import scala.concurrent.duration.DurationInt
import scala.util.Using

class NetworkAndQueueIssuesTestSuite extends WsSuiteBase with HasWebSockets with HasKafka {

  private val requestTimeout = 15.seconds
  private val maxFailures = 5

  // Hacks, see DEX-794
  private val deliveryTimeout = requestTimeout + 1.second
  private val waitAfterNetworkChanges = deliveryTimeout

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex {
  price-assets = [ "$UsdId", "WAVES" ]
  events-queue {
    kafka.producer.client {
      acks = 1
      request.timeout.ms = ${requestTimeout.toMillis}
      delivery.timeout.ms = ${deliveryTimeout.toMillis}
      connections.max.idle.ms = ${deliveryTimeout.toMillis}
    }

    circuit-breaker {
      max-failures = $maxFailures
      reset-timeout = 2000ms
    }
  }
}""")

  private val topicName = s"test-${ThreadLocalRandom.current.nextInt(0, Int.MaxValue)}"
  override protected lazy val dexRunConfig = dexKafkaConfig(topicName).withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    kafka.start()

    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "Matcher should able to restore the work after kafka issues solved" in {
    placeAndAwaitAtDex(mkOrderDP(alice, wavesUsdPair, SELL, 1.waves, 3.0))

    disconnectKafkaFromNetwork()
    Thread.sleep(waitAfterNetworkChanges.toMillis)

    val offsetBefore = dex1.api.getCurrentOffset

    (1 to maxFailures).foreach { i =>
      dex1.tryApi.place(mkOrderDP(alice, wavesUsdPair, SELL, i.waves, 3.0)) shouldBe Symbol("left")
    }

    Thread.sleep(requestTimeout.toMillis)
    connectKafkaToNetwork()
    Thread.sleep(waitAfterNetworkChanges.toMillis)

    withClue("Messages weren't saved to the queue") {
      dex1.api.getCurrentOffset shouldBe offsetBefore
    }

    dex1.tryApi.place(mkOrderDP(alice, wavesUsdPair, SELL, 1.waves, 3.0)) shouldBe Symbol("right")

    dex1.api.cancelAllOrdersWithSig(alice)
  }

  "Matcher should free reserved balances if order wasn't placed into the queue" in {

    val initialWavesBalance: Double = denormalizeWavesAmount(wavesNode1.api.balance(alice, Waves)).toDouble
    val initialUsdBalance: Double = denormalizeAmountAndFee(wavesNode1.api.balance(alice, usd), 2).toDouble

    Using.resource(mkWsAddressConnection(alice, dex1)) { wsac =>

      assertChanges(wsac, squash = false)(Map(Waves -> WsBalances(initialWavesBalance, 0), usd -> WsBalances(initialUsdBalance, 0)))()

      val sellOrder = mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 3.0)
      placeAndAwaitAtDex(sellOrder)

      dex1.api.getReservedBalanceWithApiKey(alice) should matchTo(Map[Asset, Long](Waves -> 10.003.waves))

      assertChanges(wsac)(Map(Waves -> WsBalances(initialWavesBalance - 10.003, 10.003))) {
        WsOrderConverter.fromDomain(LimitOrder(sellOrder))
      }

      disconnectKafkaFromNetwork()
      Thread.sleep(waitAfterNetworkChanges.toMillis)

      dex1.tryApi.cancelOneOrAllInPairOrdersWithSig(alice, sellOrder) shouldBe Symbol("left")

      val bigSellOrder = mkOrderDP(alice, wavesUsdPair, SELL, 30.waves, 3.0)
      dex1.tryApi.place(bigSellOrder) shouldBe Symbol("left")

      dex1.api.getReservedBalanceWithApiKey(alice) should matchTo(Map[Asset, Long](Waves -> 10.003.waves))

      assertChanges(wsac, squash = false)(
        Map(Waves -> WsBalances(initialWavesBalance - 40.006, 40.006)),
        Map(Waves -> WsBalances(initialWavesBalance - 10.003, 10.003))
      )()

      val oh = dex1.api.getOrderHistoryByPKWithSig(alice, Some(true))
      oh should have size 1
      oh.head.id shouldBe sellOrder.id()

      connectKafkaToNetwork()
      Thread.sleep(waitAfterNetworkChanges.toMillis)

      dex1.tryApi.cancelOneOrAllInPairOrdersWithSig(alice, sellOrder) shouldBe Symbol("right")
      dex1.api.waitForOrderStatus(sellOrder, Status.Cancelled)

      dex1.api.getOrderHistoryByPKWithSig(alice, Some(true)) should have size 0
      dex1.api.getReservedBalanceWithApiKey(alice) shouldBe empty

      assertChanges(wsac, squash = false)(Map(Waves -> WsBalances(initialWavesBalance, 0))) {
        WsOrder.fromOrder(sellOrder, status = OrderStatusNames.CANCELLED.some)
      }

      dex1.tryApi.place(bigSellOrder) shouldBe Symbol("right")
      dex1.api.waitForOrderStatus(bigSellOrder, Status.Accepted)

      dex1.api.getOrderHistoryByPKWithSig(alice, Some(true)) should have size 1
      dex1.api.getReservedBalanceWithApiKey(alice) should matchTo(Map[Asset, Long](Waves -> 30.003.waves))

      assertChanges(wsac, squash = false)(Map(Waves -> WsBalances(initialWavesBalance - 30.003, 30.003))) {
        WsOrderConverter.fromDomain(LimitOrder(bigSellOrder))
      }

      dex1.api.cancelAllOrdersWithSig(alice)
    }
  }

  "Matcher should stop working with appropriate error if the queue was " - {
    "switched" in {
      fillQueueAndSaveSnapshots()

      try {
        dex1.restartWithNewSuiteConfig(
          ConfigFactory
            .parseString("""waves.dex {
                            events-queue.kafka.topic = new-topic
                            snapshots-interval = 3
                          }""")
            .withFallback(dexInitialSuiteConfig)
        )
        fail("Expected Matcher stopped with the exit code of 12")
      } catch {
        case _: Throwable =>
          dex1.getState().getExitCodeLong shouldBe 12 // RecoveryError.code
      } // Add finally (above) if you write a new test
    }
  }

  private def fillQueueAndSaveSnapshots(): Unit = {
    val offsetBefore = dex1.api.getLastOffset

    val orders = (1 to 10).map(i => mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 3.0, ttl = i.days))
    orders.foreach(placeAndAwaitAtDex(_))

    orders.foreach(dex1.api.cancelOneOrAllInPairOrdersWithSig(alice, _))
    dex1.api.waitForOrderHistory(alice, true.some)(_.isEmpty)

    dex1.api.saveSnapshots
    eventually {
      dex1.api.getAllSnapshotOffsets(wavesUsdPair) shouldBe (offsetBefore + 20) // 10 orders * 2 (place and cancel)
    }
  }

}
