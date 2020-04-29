package com.wavesplatform.it.sync

import java.util.concurrent.ThreadLocalRandom

import com.dimafeng.testcontainers.KafkaContainer
import com.github.dockerjava.api.model.ContainerNetwork
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.it.MatcherSuiteBase

import scala.collection.JavaConverters._

class KafkaIssuesTestSuite extends MatcherSuiteBase {

  private val kafkaContainerName = "kafka"
  private val kafkaIp            = getIp(12)

  private val kafka: KafkaContainer =
    KafkaContainer().configure { k =>
      k.withNetwork(network)
      k.withNetworkAliases(kafkaContainerName)
      k.withCreateContainerCmdModifier { cmd =>
        cmd withName kafkaContainerName
        cmd withIpv4Address getIp(12)
      }
    }

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""")

  override protected lazy val dexRunConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.events-queue {
       |  type = kafka
       |  kafka {
       |    servers = "$kafkaIp:9092"
       |    topic = "dex-${ThreadLocalRandom.current.nextInt(0, Int.MaxValue)}"
       |  }
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    kafka.start()

    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  private def disconnectKafkaFromNetwork(): Unit = {
    kafka.dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(kafka.containerId)
      .withNetworkId(kafka.network.getId)
      .exec()
  }

  private def connectKafkaToNetwork(): Unit = {
    kafka.dockerClient
      .connectToNetworkCmd()
      .withContainerId(kafka.containerId)
      .withNetworkId(kafka.network.getId)
      .withContainerNetwork(
        new ContainerNetwork()
          .withIpamConfig(new ContainerNetwork.Ipam().withIpv4Address(kafkaIp))
          .withAliases(kafka.networkAliases.asJava))
      .exec()
  }

  "Matcher should free reserved balances if order wasn't placed into the queue" in {

    val order = mkOrderDP(alice, wavesUsdPair, SELL, 10.waves, 3.0)
    placeAndAwaitAtDex(order)

    dex1.api.currentOffset shouldBe 0
    dex1.api.reservedBalance(alice) should matchTo(Map[Asset, Long](Waves -> 10.003.waves))

    disconnectKafkaFromNetwork()

    dex1.api.tryCancel(alice, order) shouldBe 'left
    dex1.api.tryPlace(mkOrderDP(alice, wavesUsdPair, SELL, 30.waves, 3.0)) shouldBe 'left

    dex1.api.reservedBalance(alice) should matchTo(Map[Asset, Long](Waves -> 10.003.waves))

    val oh = dex1.api.orderHistory(alice, Some(true))
    oh should have size 1
    oh.head.id shouldBe order.id()

    connectKafkaToNetwork()

    dex1.api.tryCancel(alice, order) shouldBe 'right
    dex1.api.orderHistory(alice, Some(true)) should have size 0
    dex1.api.reservedBalance(alice) shouldBe empty

    dex1.api.tryPlace(mkOrderDP(alice, wavesUsdPair, SELL, 30.waves, 3.0)) shouldBe 'right
    dex1.api.orderHistory(alice, Some(true)) should have size 1
    dex1.api.reservedBalance(alice) should matchTo(Map[Asset, Long](Waves -> 30.003.waves))
  }
}
