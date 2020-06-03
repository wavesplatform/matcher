package com.wavesplatform.it.sync.networking

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.SELL
import com.wavesplatform.dex.it.api.HasToxiProxy
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.it.WsSuiteBase
import com.wavesplatform.it.tags.NetworkTests
import eu.rekawek.toxiproxy.model.ToxicDirection
import org.testcontainers.containers.ToxiproxyContainer.ContainerProxy

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

@NetworkTests
class NetworkIssuesTestSuite extends WsSuiteBase with HasToxiProxy {

  private val wavesNodeProxy: ContainerProxy = mkToxiProxy(WavesNodeContainer.wavesNodeNetAlias, WavesNodeContainer.dexGrpcExtensionPort)

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory
      .parseString(
        s"""waves.dex {
           |  price-assets = [ "$UsdId", "WAVES" ]
           |  waves-blockchain-client.grpc.target = "$toxiProxyHostName:${getInnerToxiProxyPort(wavesNodeProxy)}"
           |}""".stripMargin
      )
      .withFallback(jwtPublicKeyConfig)

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

  "DEXClient should place orders despite of short time disconnect from network" in {

    val orders = (0 to 100).map { i =>
      mkOrder(alice, wavesUsdPair, OrderType.SELL, 1.waves, 100 + i)
    }

    Await.ready(
      for {
        _ <- Future.successful(())

        _ <- {
          orders.foreach(dex1.asyncApi.place(_))
          Future.successful()
        }

        _ <- {
          Thread.sleep(500)
          dex1.disconnectFromNetwork()
          Thread.sleep(500)
          dex1.connectToNetwork()
          Future.successful()
        }

        orderBook <- dex1.asyncApi.orderBook(wavesUsdPair)

      } yield {
        orderBook.asks should have size 100
        orders.foreach(dex1.asyncApi.waitForOrderStatus(_, OrderStatus.Accepted))
      },
      2.minute
    )

    dex1.api.cancelAllByPair(alice, wavesUsdPair)
  }

  "DEXClient should obtain balance changes when it reconnects after losing connection: " - {

    "user has a balances snapshot (got by ws connection)" in {
      val acc = mkAccountWithBalance(100.waves -> Waves)
      val wsc = mkWsAddressConnection(acc, dex1)

      eventually { wsc.addressStateChanges should have size 1 }

      wsc.close()
      dex1.disconnectFromNetwork()

      broadcastAndAwait(mkTransfer(acc, alice.toAddress, 99.waves, Waves))
      wavesNode1.api.balance(acc.toAddress, Waves) should be(0.999.waves)

      dex1.connectToNetwork()

      dex1.api.tryPlace(mkOrder(acc, wavesUsdPair, SELL, 50.waves, 1.usd)) should failWith(3147270)
    }

    "user doesn't have a balances snapshot (got by ws connection)" in {
      val acc = mkAccountWithBalance(100.waves -> Waves)

      dex1.disconnectFromNetwork()

      broadcastAndAwait(mkTransfer(acc, alice.toAddress, 99.waves, Waves))
      wavesNode1.api.balance(acc.toAddress, Waves) should be(0.999.waves)

      dex1.connectToNetwork()

      dex1.api.tryPlace(mkOrder(acc, wavesUsdPair, SELL, 50.waves, 1.usd)) should failWith(3147270)
    }
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

  "DEXClient should connect to another node from pool if linked node had lost the connection to network " in {

    val conf = ConfigFactory.parseString(s"""waves.dex {
                                 |  price-assets = [ "$UsdId", "WAVES" ]
                                 |  waves-blockchain-client.grpc.target = "${WavesNodeContainer.wavesNodeNetAlias}:${WavesNodeContainer.dexGrpcExtensionPort}"
                                 |}""".stripMargin)

    dex1.restartWithNewSuiteConfig(conf)

    val account = mkAccountWithBalance(5.004.waves -> Waves)

    markup("Place order")
    val order = mkOrder(account, wavesUsdPair, SELL, 5.waves, 5.usd)
    placeAndAwaitAtDex(order)

    markup("Up node 2")
    wavesNode2.start()
    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)

    wavesNode2.api.waitForHeight(wavesNode1.api.currentHeight)
    wavesNode2.api.waitForTransaction(IssueUsdTx)

    wavesNode1.disconnectFromNetwork()

    Thread.sleep(25000) // waiting for dex's grpc connection to node-2

    broadcastAndAwait(wavesNode2.api, mkTransfer(account, bob, amount = 4.waves, asset = Waves))

    markup("Now DEX receives balances stream from the node 2 and cancels order")
    dex1.api.waitForOrderStatus(order, OrderStatus.Cancelled)

    markup("Place order")
    placeAndAwaitAtDex(mkOrder(account, wavesUsdPair, SELL, 1.waves, 5.usd))
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
