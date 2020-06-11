package com.wavesplatform.it.sync

import com.github.dockerjava.api.model.{ContainerNetwork, NetworkSettings}
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.websockets.{WsBalances, WsOrder}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.dex.it.api.responses.dex.{OrderStatus => ApiOrderStatus}
import com.wavesplatform.dex.it.api.websockets.HasWebSockets
import com.wavesplatform.dex.model.{LimitOrder, OrderStatus}
import com.wavesplatform.it.WsSuiteBase
import com.wavesplatform.it.tags.DexItKafkaRequired

import scala.collection.JavaConverters._

@DexItKafkaRequired
class KafkaIssuesTestSuite extends WsSuiteBase with HasWebSockets with HasKafka {

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex { 
  price-assets = [ "$UsdId", "WAVES" ]
  events-queue.kafka.producer.client {
    request.timeout.ms = 1000
    delivery.timeout.ms = 1100
  }
}""")

  override protected lazy val dexRunConfig = dexKafkaConfig().withFallback(jwtPublicKeyConfig)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    kafka.start()

    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  private val networkId = kafka.network.getId

  "Matcher should free reserved balances if order wasn't placed into the queue" in {

    val initialWavesBalance: Double = denormalizeWavesAmount(wavesNode1.api.balance(alice, Waves)).toDouble
    val initialUsdBalance: Double   = denormalizeAmountAndFee(wavesNode1.api.balance(alice, usd), 2).toDouble

    val wsac = mkWsAddressConnection(alice, dex1)

    assertChanges(wsac, squash = false) { Map(Waves -> WsBalances(initialWavesBalance, 0), usd -> WsBalances(initialUsdBalance, 0)) }()

    val sellOrder = mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 3.0)
    placeAndAwaitAtDex(sellOrder)

    dex1.api.currentOffset shouldBe 0
    dex1.api.reservedBalance(alice) should matchTo(Map[Asset, Long](Waves -> 10.003.waves))

    assertChanges(wsac) { Map(Waves -> WsBalances(initialWavesBalance - 10.003, 10.003)) } {
      WsOrder.fromDomain(LimitOrder(sellOrder), OrderStatus.Accepted)
    }

    disconnectKafkaFromNetwork()

    dex1.api.tryCancel(alice, sellOrder) shouldBe 'left

    val bigSellOrder = mkOrderDP(alice, wavesUsdPair, SELL, 30.waves, 3.0)
    dex1.api.tryPlace(bigSellOrder) shouldBe 'left

    dex1.api.reservedBalance(alice) should matchTo(Map[Asset, Long](Waves -> 10.003.waves))

    assertChanges(wsac, squash = false)(
      Map(Waves -> WsBalances(initialWavesBalance - 40.006, 40.006)),
      Map(Waves -> WsBalances(initialWavesBalance - 10.003, 10.003)),
    )()

    val oh = dex1.api.orderHistory(alice, Some(true))
    oh should have size 1
    oh.head.id shouldBe sellOrder.id()

    connectKafkaToNetwork()

    dex1.api.tryCancel(alice, sellOrder) shouldBe 'right
    dex1.api.waitForOrderStatus(sellOrder, ApiOrderStatus.Cancelled)

    dex1.api.orderHistory(alice, Some(true)) should have size 0
    dex1.api.reservedBalance(alice) shouldBe empty

    assertChanges(wsac, squash = false) { Map(Waves -> WsBalances(initialWavesBalance, 0)) } {
      WsOrder(id = sellOrder.id(), status = OrderStatus.Cancelled.name)
    }

    dex1.api.tryPlace(bigSellOrder) shouldBe 'right
    dex1.api.waitForOrderStatus(bigSellOrder, ApiOrderStatus.Accepted)

    dex1.api.orderHistory(alice, Some(true)) should have size 1
    dex1.api.reservedBalance(alice) should matchTo(Map[Asset, Long](Waves -> 30.003.waves))

    assertChanges(wsac, squash = false) { Map(Waves -> WsBalances(initialWavesBalance - 30.003, 30.003)) } {
      WsOrder.fromDomain(LimitOrder(bigSellOrder), OrderStatus.Accepted)
    }

    dex1.api.cancelAll(alice)
    wsac.close()
  }

  private def disconnectKafkaFromNetwork(): Unit = {
    writeGlobalLog("--- Disconnecting Kafka from the network ---")
    kafka.dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(kafka.containerId)
      .withNetworkId(networkId)
      .exec()

    waitForNetworkSettings(_.getNetworks.containsKey(networkId))
  }

  private def connectKafkaToNetwork(): Unit = {
    writeGlobalLog("--- Connecting Kafka to the network ---")
    kafka.dockerClient
      .connectToNetworkCmd()
      .withContainerId(kafka.containerId)
      .withNetworkId(networkId)
      .withContainerNetwork(
        new ContainerNetwork()
          .withIpamConfig(new ContainerNetwork.Ipam().withIpv4Address(kafkaIp))
          .withAliases(kafka.networkAliases.asJava))
      .exec()

    waitForNetworkSettings(!_.getNetworks.containsKey(networkId))
  }

  private def waitForNetworkSettings(pred: NetworkSettings => Boolean): Unit =
    Iterator
      .continually {
        Thread.sleep(1000)
        kafka.dockerClient.inspectContainerCmd(kafka.containerId).exec().getNetworkSettings
      }
      .zipWithIndex
      .find { case (ns, attempt) => pred(ns) || attempt == 10 }
      .fold(log.warn(s"Can't wait on ${kafka.containerId}"))(_ => ())
}
