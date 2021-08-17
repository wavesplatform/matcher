package com.wavesplatform.it.sync.networking

import com.github.dockerjava.api.command.CreateNetworkCmd
import com.github.dockerjava.api.model.ContainerNetwork
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.docker.{DexContainer, WavesNodeContainer}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItExternalKafkaRequired
import org.testcontainers.containers.Network

import java.util.concurrent.ThreadLocalRandom
import scala.jdk.CollectionConverters.MapHasAsScala

@DexItExternalKafkaRequired
class MultipleMatchersOrderCancelTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  private lazy val internalNetwork =
    Network
      .builder()
      .createNetworkCmdModifier { cmd: CreateNetworkCmd =>
        cmd
          .withName(s"MultipleMatchersOrderCancelTestSuite${ThreadLocalRandom.current().nextInt()}")
          .withInternal(true) // Disable internet, thus break Kafka connection
      }
      .build()

  // Matchers will connect to this Node through the internalNetwork
  override protected lazy val wavesNode1: WavesNodeContainer = createWavesNode("waves-1", netAlias = None)
  protected lazy val dex2: DexContainer = createDex("dex-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    val containerNetwork = new ContainerNetwork().withNetworkID(internalNetwork.getId).withAliases(WavesNodeContainer.wavesNodeNetAlias)

    dex1.dockerClient
      .connectToNetworkCmd()
      .withContainerId(wavesNode1.containerId)
      .withContainerNetwork(containerNetwork)
      .exec()

    broadcastAndAwait(IssueUsdTx, IssueEthTx)

    dex1.beginStart()
    dex1.dockerClient
      .connectToNetworkCmd()
      .withContainerId(dex1.containerId)
      .withContainerNetwork(new ContainerNetwork().withNetworkID(internalNetwork.getId))
      .exec()
    dex1.endStart()

    dex2.beginStart()
    dex2.dockerClient
      .connectToNetworkCmd()
      .withContainerId(dex2.containerId)
      .withContainerNetwork(new ContainerNetwork().withNetworkID(internalNetwork.getId))
      .exec()
    dex2.endStart()
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

    val ts = System.currentTimeMillis()
    val sellOrders = (0 to 4).map { i =>
      val amt = i + 1
      mkOrderDP(acc1, wavesUsdPair, OrderType.SELL, amt.waves, amt, ts = ts + amt) // To cancel latest first
    }

    sellOrders.foreach(placeAndAwaitAtDex(_))

    // if DEX-1 will work with local queue, it won't receive buy orders placements and
    // will cancel remained orders due to balance changes
    // (which were caused by exchange transactions from DEX-2)

    val inspect = dex1.dockerClient
      .inspectContainerCmd(dex1.containerId)
      .exec()

    val (defaultNetworkName, defaultNetworkId) = inspect.getNetworkSettings.getNetworks.asScala
      .collectFirst {
        case (name, network) if name.startsWith("waves-") => (name, network.getNetworkID)
      }
      .getOrElse(throw new RuntimeException("Can't find network"))

    step(s"dex1: disconnecting from $defaultNetworkName")

    dex1.dockerClient
      .disconnectFromNetworkCmd()
      .withContainerId(dex1.container.getContainerId)
      .withNetworkId(defaultNetworkId)
      .exec()

    Iterator
      .continually {
        Thread.sleep(1000)
        log.info(s"dex1: checking disconnected from $defaultNetworkName/$defaultNetworkId")
        dex1.dockerClient
          .inspectContainerCmd(dex1.containerId)
          .exec()
      }
      .dropWhile(_.getNetworkSettings.getNetworks.containsKey(defaultNetworkName))
      .take(1)
      .foreach { _ =>
        step("dex1: disconnected")
      }

    val submittedOrders = (0 to 2).map { i =>
      val amt = i + 1
      mkOrderDP(acc2, wavesUsdPair, OrderType.BUY, amt.waves, amt)
    }
    submittedOrders.foreach(placeAndAwaitAtDex(_, Status.Filled, dex2))
    submittedOrders.foreach(waitForOrderAtNode(_, dex2.api))

    step("Orders placed")

    // Enough to find exchange transactions in UTX and cancel orders #3-4
    Thread.sleep(5000)

    (0 to 2).foreach { i =>
      dex2.api.orderStatusByAssetPairAndId(sellOrders(i)).status shouldBe Status.Filled
    }

    // Matcher should not cancel rest orders!
    withClue("Matcher should not cancel rest orders: ") {
      (3 to 4).foreach { i =>
        dex2.api.orderStatusByAssetPairAndId(sellOrders(i)).status shouldBe Status.Accepted
      }
    }
  }
}
