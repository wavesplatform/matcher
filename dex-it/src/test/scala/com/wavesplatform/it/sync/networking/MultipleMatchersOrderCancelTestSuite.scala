package com.wavesplatform.it.sync.networking

import com.github.dockerjava.api.command.CreateNetworkResponse
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.docker.DexContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.DexItExternalKafkaRequired

import java.util.concurrent.ThreadLocalRandom
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.Using
import scala.util.Using.Releasable

@DexItExternalKafkaRequired
class MultipleMatchersOrderCancelTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |}""".stripMargin
  )

  protected lazy val dex2: DexContainer = createDex("dex-2")

  private lazy val internalNetwork = dex1.dockerClient
    .createNetworkCmd()
    .withDriver("bridge")
    .withName(s"MultipleMatchersOrderCancelTestSuite${ThreadLocalRandom.current().nextInt()}")
    .withInternal(true) // Disable internet, thus break Kafka connection
    .exec()

  private lazy val containers = List(wavesNode1.container, dex1.container, dex2.container)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueEthTx)
    dex1.start()
    dex2.start()
    containers.foreach { container =>
      dex1.dockerClient
        .connectToNetworkCmd()
        .withContainerId(container.getContainerId)
        .withNetworkId(internalNetwork.getId)
        .exec()
    }
  }

  override protected def afterAll(): Unit = {
    containers.foreach { container =>
      dex1.dockerClient
        .disconnectFromNetworkCmd()
        .withContainerId(container.getContainerId)
        .withNetworkId(internalNetwork.getId)
        .exec()
    }
    super.afterAll()
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
        step("dex1: isolated")
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
