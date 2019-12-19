package com.wavesplatform.it.sync

import cats.Id
import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.node.NodeApi
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.docker.base.info.WavesNodeContainerInfo
import com.wavesplatform.dex.it.docker.{ToxiProxy, base}
import com.wavesplatform.dex.it.fp
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.transaction.assets.exchange.OrderType
import monix.eval.Coeval
import org.testcontainers.containers.ToxiproxyContainer.ContainerProxy

import scala.util.Try

class DEXClientFaultToleranceTestSuite extends NewMatcherSuiteBase {

  val toxiproxy: ToxiProxy  = new ToxiProxy().start()
  val proxy: ContainerProxy = toxiproxy.container.getProxy(WavesNodeContainerInfo.netAlias, WavesNodeContainerInfo.dexGrpcExtensionPort)

  override protected def dexInitialSuiteConfig: Config = {
    ConfigFactory.parseString(s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  grpc.integration {
         |    waves-node-grpc {
         |      host = ${toxiproxy.name}
         |      port = ${toxiproxy.getInnerProxyPort(proxy)}
         |    }
         |  }
         |}""".stripMargin)
  }

  private val wavesNode2Container: Coeval[base.WavesNodeContainer] = Coeval.evalOnce { createWavesNode("waves-2") }

  protected val cachedWavesNode2ApiAddress = CachedData(wavesNode2Container().restApiAddress)

  private def wavesNode2Api: NodeApi[Id] = fp.sync(NodeApi[Try]("integration-test-rest-api", cachedWavesNode2ApiAddress.get()))

  override protected def beforeAll(): Unit = {
    startAndWait(wavesNodeContainer(), wavesNodeApi)
    broadcastAndAwait(IssueUsdTx)
    startAndWait(dexContainer(), dexApi)
  }

  "DEXClient should works correctly despite of the short connection losses" in {

    val aliceBuyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNodeApi.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNodeApi.balance(bob, usd), asset = usd)

    markup("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1")
    dexApi.place(aliceBuyOrder)
    dexApi.waitForOrderStatus(aliceBuyOrder, OrderStatus.Accepted)

    markup(s"Disconnect DEX from the network and perform USD transfer from Alice to Bob")
    proxy.setConnectionCut(true)

    broadcastAndAwait(wavesNodeApi, alice2BobTransferTx)
    usdBalancesShouldBe(wavesNodeApi, 0, defaultAssetQuantity)

    Thread.sleep(5000)

    markup("Connect DEX back to the network, DEX should know about transfer and cancel Alice's order")
    proxy.setConnectionCut(false)
    invalidateCaches()

    dexApi.waitForOrderStatus(aliceBuyOrder, OrderStatus.Cancelled)

    withClue("Cleanup") {
      broadcastAndAwait(wavesNodeApi, bob2AliceTransferTx)
    }
  }

  "DEXClient should switch nodes if connection to one of them was lost due to node shutdown" in {

    // also works for the cases when nodes are disconnected from the network (not stopped),
    // in these cases some delays after disconnections are required

    val aliceBuyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)
    val bobBuyOrder   = mkOrder(bob, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNode2Api.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNodeApi.balance(bob, usd), asset = usd)

    markup("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1")
    dexApi.place(aliceBuyOrder)
    dexApi.waitForOrderStatus(aliceBuyOrder, OrderStatus.Accepted)

    markup("Up node 2")
    wavesNode2Container().start()

    wavesNode2Api.waitReady
    wavesNode2Api.connect(wavesNodeNetworkApiAddress)
    wavesNode2Api.waitForConnectedPeer(wavesNodeNetworkApiAddress)

    wavesNode2Api.waitForTransaction(IssueUsdTx)

    markup(s"Stop node 1 and perform USD transfer from Alice to Bob")
    wavesNodeContainer().stopAndSaveLogs

    broadcastAndAwait(wavesNode2Api, alice2BobTransferTx)
    usdBalancesShouldBe(wavesNode2Api, expectedAliceBalance = 0, expectedBobBalance = defaultAssetQuantity)

    markup("Now DEX receives balances stream from the node 2 and cancels Alice's order")
    dexApi.waitForOrderStatus(aliceBuyOrder, OrderStatus.Cancelled)

    markup("Bob places order that requires some amount of USD, DEX receives balances stream from the node 2")
    dexApi.place(bobBuyOrder)
    dexApi.waitForOrderStatus(bobBuyOrder, OrderStatus.Accepted)

    markup("Up node 1")
    wavesNodeContainer().start()
    invalidateCaches()

    wavesNodeApi.waitReady
    wavesNode2Api.connect(wavesNodeNetworkApiAddress)
    wavesNode2Api.waitForConnectedPeer(wavesNodeNetworkApiAddress)
    wavesNodeApi.waitForTransaction(alice2BobTransferTx)

    markup(s"Stop node 2 and perform USD transfer from Bob to Alice")
    wavesNode2Container().stopAndSaveLogs
    forgetContainer(wavesNode2Container())

    broadcastAndAwait(wavesNodeApi, bob2AliceTransferTx)
    usdBalancesShouldBe(wavesNodeApi, defaultAssetQuantity, 0)

    markup("Now DEX receives balances stream from the node 1 and cancels Bob's order")
    dexApi.waitForOrderStatus(bobBuyOrder, OrderStatus.Cancelled)
  }

  "DEXClient should correctly handle gRPC errors" in {

    val order = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    wavesNodeContainer().disconnectFromNetwork()

    dexApi.tryPlace(order) should failWith(
      105906177,
      "Waves Node is unavailable, please retry later or contact with the administrator"
    )

    wavesNodeContainer().connectToNetwork()
    invalidateCaches()
    wavesNodeApi.waitReady

    dexApi.retryPlace(order)

    dexApi.waitForOrderStatus(order, OrderStatus.Accepted)
  }

  private def usdBalancesShouldBe(wavesNodeApi: NodeApi[Id], expectedAliceBalance: Long, expectedBobBalance: Long): Unit = {
    withClue("alice:")(wavesNodeApi.balance(alice, usd) shouldBe expectedAliceBalance)
    withClue("bob:")(wavesNodeApi.balance(bob, usd) shouldBe expectedBobBalance)
  }

  override protected def invalidateCaches(): Unit = {
    super.invalidateCaches()
    cachedWavesNode2ApiAddress.invalidate()
  }
}
