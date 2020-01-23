package com.wavesplatform.it.sync.networking

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.HasToxiProxy
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.NetworkTests
import eu.rekawek.toxiproxy.model.ToxicDirection
import org.testcontainers.containers.ToxiproxyContainer.ContainerProxy

@NetworkTests
class NetworkIssuesTestSuite extends MatcherSuiteBase with HasToxiProxy {

  private val wavesNodeProxy: ContainerProxy = mkToxiProxy(WavesNodeContainer.netAlias, WavesNodeContainer.dexGrpcExtensionPort)

  override protected def dexInitialSuiteConfig: Config = {
    ConfigFactory.parseString(s"""waves.dex {
                                 |  price-assets = [ "$UsdId", "WAVES" ]
                                 |  waves-blockchain-client.grpc.target = "$toxiProxyHostName:${getInnerToxiProxyPort(wavesNodeProxy)}"
                                 |}""".stripMargin)
  }

  lazy val wavesNode2: WavesNodeContainer = createWavesNode("waves-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  override protected def afterEach(): Unit = {
    wavesNodeProxy.toxics().getAll.forEach(_.remove())
    clearOrderBook()
  }

  "DEXClient should works correctly despite of latency: " - {

    "high latency (from node to dex)" in {
      wavesNodeProxy.toxics().latency("latency", ToxicDirection.DOWNSTREAM, 4500)
      makeAndMatchOrders()
      matchingShouldBeSuccess()
    }

    "high latency (from dex to node)" in {
      wavesNodeProxy.toxics().latency("latency", ToxicDirection.UPSTREAM, 4500)
      makeAndMatchOrders()
      matchingShouldBeSuccess()
    }

    "high latency (both directions)" in {
      wavesNodeProxy.toxics().latency("latencyD", ToxicDirection.DOWNSTREAM, 4500)
      wavesNodeProxy.toxics().latency("latencyU", ToxicDirection.UPSTREAM, 4500)
      makeAndMatchOrders()
      matchingShouldBeSuccess()
    }
  }

  "DEXClient should works correctly despite of slow network: " - {

    "16 kbps from node to dex" in {
      wavesNodeProxy.toxics().bandwidth("bandwidth", ToxicDirection.DOWNSTREAM, 16)
      makeAndMatchOrders()
      matchingShouldBeSuccess()
    }

    "16 kbps from dex to node" in {
      wavesNodeProxy.toxics().bandwidth("bandwidth", ToxicDirection.UPSTREAM, 16)
      makeAndMatchOrders()
      matchingShouldBeSuccess()
    }

    "16 kbps in both directions" in {
      wavesNodeProxy.toxics().bandwidth("bandwidthD", ToxicDirection.DOWNSTREAM, 16)
      wavesNodeProxy.toxics().bandwidth("bandwidthU", ToxicDirection.UPSTREAM, 16)
      makeAndMatchOrders()
      matchingShouldBeSuccess()
    }
  }

  private def clearOrderBook(): Unit = {
    dex1.api.cancelAll(alice)
    dex1.api.cancelAll(bob)
  }

  private def makeAndMatchOrders(): Unit = {
    for (i <- 1 to 5) {
      val o1 = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, i * 300)
      val o2 = mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, i * 300)
      dex1.api.place(o1)
      dex1.api.place(o2)
    }
  }

  private def matchingShouldBeSuccess(): Unit = {
    val ob = dex1.api.orderBook(wavesUsdPair)
    ob.bids should be(empty)
    ob.asks should be(empty)
  }
}
