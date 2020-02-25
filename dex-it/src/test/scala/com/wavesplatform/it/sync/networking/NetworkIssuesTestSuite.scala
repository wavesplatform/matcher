package com.wavesplatform.it.sync.networking

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.HasToxiProxy
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.docker.{DexContainer, WavesNodeContainer}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.NetworkTests
import org.scalatest
import org.testcontainers.containers.ToxiproxyContainer

@NetworkTests
class NetworkIssuesTestSuite extends MatcherSuiteBase with HasToxiProxy {

  private val dex1KafkaProxy: ToxiproxyContainer.ContainerProxy = mkToxiProxy("kafka-dev-aws-fr-1.wvservices.com", 9092)

  override protected def dexInitialSuiteConfig(): Config = {
    ConfigFactory.parseString(s"""waves.dex {
                                 |  events-queue {
                                 |      type = "kafka"
                                 |      kafka {
                                 |          servers = "$toxiProxyHostName:${getInnerToxiProxyPort(dex1KafkaProxy)}"
                                 |          topic = "$toxiProxyHostName"
                                 |          group = "$toxiProxyHostName"
                                 |     }
                                 |  }
                                 |  price-assets = [ "$UsdId", "WAVES" ]
                                 |}""".stripMargin)
  }

  protected def dex2InitialSuiteConfig: Config = {
    ConfigFactory.parseString(s"""waves.dex {
                                 |
                                 |  events-queue {
                                 |      type = "kafka"
                                 |      kafka {
                                 |          servers = "kafka-dev-aws-fr-1.wvservices.com:9092"
                                 |          topic = "$toxiProxyHostName"
                                 |          group = "$toxiProxyHostName"
                                 |     }
                                 |  }
                                 |  price-assets = [ "$UsdId", "WAVES" ]
                                 |}""".stripMargin)
  }

  protected lazy val dex2: DexContainer   = createDex("dex-2", suiteInitialConfig = dex2InitialSuiteConfig)
  lazy val wavesNode2: WavesNodeContainer = createWavesNode("waves-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
    dex2.start()
  }

  override protected def afterEach(): Unit = {
    //   wavesNodeProxy.toxics().getAll.forEach(_.remove())
    //  clearOrderBook()
  }

  def createAccountWithBalance(balances: (Long, Asset)*): KeyPair = {
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

  "test with kafka" in {

    val acc1 = createAccountWithBalance(15.015.waves -> Waves)
    val acc2 = createAccountWithBalance(0.015.waves  -> Waves, 15.usd -> usd)

    (1 to 5).foreach { amt =>
      placeAndAwaitAtDex(mkOrderDP(acc1, wavesUsdPair, OrderType.SELL, amt.waves, amt))
    }

    dex1.dockerClient.disconnectFromNetworkCmd().withNetworkId(network.getId).withContainerId(getProxyContainer.getContainerId).exec()
//    dex1KafkaProxy.setConnectionCut(true)
    Thread.sleep(3000)

    (1 to 3).foreach { amt =>
      val order = mkOrderDP(acc2, wavesUsdPair, OrderType.BUY, amt.waves, amt)
      dex2.api.place(order)
      dex2.api.waitForOrderStatus(order, OrderStatus.Filled)
    }

    val orderBook = dex1.api.orderBook(wavesUsdPair)
    orderBook.bids should be(empty)
    orderBook.asks should be(empty)

    wavesNode1.api.balance(acc1, Waves) should be(0)
    wavesNode1.api.balance(acc1, usd) should be(15.usd)

    wavesNode1.api.balance(acc2, Waves) should be(15.waves)
    wavesNode1.api.balance(acc2, usd) should be(0)

  }

//  "DEXClient should works correctly despite of latency: " - {
//
//    "high latency (from node to dex)" in {
//      wavesNodeProxy.toxics().latency("latency", ToxicDirection.DOWNSTREAM, 4500)
//      makeAndMatchOrders()
//      matchingShouldBeSuccess()
//    }
//
//    "high latency (from dex to node)" in {
//      wavesNodeProxy.toxics().latency("latency", ToxicDirection.UPSTREAM, 4500)
//      makeAndMatchOrders()
//      matchingShouldBeSuccess()
//    }
//
//    "high latency (both directions)" in {
//      wavesNodeProxy.toxics().latency("latencyD", ToxicDirection.DOWNSTREAM, 4500)
//      wavesNodeProxy.toxics().latency("latencyU", ToxicDirection.UPSTREAM, 4500)
//      makeAndMatchOrders()
//      matchingShouldBeSuccess()
//    }
//  }
//
//  "DEXClient should works correctly despite of slow network: " - {
//
//    "16 kbps from node to dex" in {
//      wavesNodeProxy.toxics().bandwidth("bandwidth", ToxicDirection.DOWNSTREAM, 16)
//      makeAndMatchOrders()
//      matchingShouldBeSuccess()
//    }
//
//    "16 kbps from dex to node" in {
//      wavesNodeProxy.toxics().bandwidth("bandwidth", ToxicDirection.UPSTREAM, 16)
//      makeAndMatchOrders()
//      matchingShouldBeSuccess()
//    }
//
//    "16 kbps in both directions" in {
//      wavesNodeProxy.toxics().bandwidth("bandwidthD", ToxicDirection.DOWNSTREAM, 16)
//      wavesNodeProxy.toxics().bandwidth("bandwidthU", ToxicDirection.UPSTREAM, 16)
//      makeAndMatchOrders()
//      matchingShouldBeSuccess()
//    }
//  }

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
