package com.wavesplatform.it.sync.networking

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.error.WavesNodeConnectionBroken
import com.wavesplatform.dex.it.api.node.NodeApi
import com.wavesplatform.dex.it.containers.{HasToxiProxy, WavesNodeContainer}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.tags.NetworkTests

@NetworkTests
class DexClientFaultToleranceTestSuite extends MatcherSuiteBase with HasToxiProxy {

  private val wavesNodeProxy = mkToxiProxy(WavesNodeContainer.wavesNodeNetAlias, WavesNodeContainer.matcherGrpcExtensionPort)

  private val blockchainUpdatesExtensionProxy =
    mkToxiProxy(WavesNodeContainer.wavesNodeNetAlias, WavesNodeContainer.blockchainUpdatesGrpcExtensionPort)

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$UsdId", "WAVES" ]
         |  waves-blockchain-client { 
         |    grpc.target = "dns:///$toxiProxyHostName:${getInnerToxiProxyPort(wavesNodeProxy)}"
         |    blockchain-updates-grpc.target = "dns:///$toxiProxyHostName:${getInnerToxiProxyPort(blockchainUpdatesExtensionProxy)}"
         |  }
         |}""".stripMargin
    )

  lazy val wavesNode2: WavesNodeContainer = createWavesNode("waves-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "DEXClient should works correctly despite of the short connection losses" in {

    val aliceBuyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNode1.api.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNode1.api.balance(bob, usd), asset = usd)

    step("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1")
    dex1.api.place(aliceBuyOrder)
    dex1.api.waitForOrderStatus(aliceBuyOrder, Status.Accepted)

    step(s"Disconnect DEX from the network and perform USD transfer from Alice to Bob")
    wavesNodeProxy.setConnectionCut(true)

    broadcastAndAwait(alice2BobTransferTx)
    usdBalancesShouldBe(wavesNode1.api, 0, defaultAssetQuantity)

    step("Connect DEX back to the network, DEX should know about transfer and cancel Alice's order")
    wavesNodeProxy.setConnectionCut(false)

    dex1.api.waitForOrderStatus(aliceBuyOrder, Status.Cancelled)

    withClue("Cleanup") {
      broadcastAndAwait(bob2AliceTransferTx)
    }
  }

  // may fail
  // wait for a task NODE-2309 and node release 1.3.7 for possible solution
  "DEXClient should switch nodes if connection to one of them was lost due to node shutdown" in {

    // also works for the cases when nodes are disconnected from the network (not stopped),
    // in these cases some delays after disconnections are required

    val aliceBuyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)
    val bobBuyOrder = mkOrder(bob, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNode2.api.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNode1.api.balance(bob, usd), asset = usd)

    step("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1")
    dex1.api.place(aliceBuyOrder)
    dex1.api.waitForOrderStatus(aliceBuyOrder, Status.Accepted)

    step("Up node 2")
    wavesNode2.start()
    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)

    wavesNode2.api.waitForTransaction(IssueUsdTx)
    wavesNode2.api.waitForHeight(wavesNode1.api.currentHeight)

    step(s"Stop node 1 and perform USD transfer from Alice to Bob")
    wavesNode1.stopWithoutRemove()

    broadcastAndAwait(wavesNode2.api, alice2BobTransferTx)
    usdBalancesShouldBe(wavesNode2.api, expectedAliceBalance = 0, expectedBobBalance = defaultAssetQuantity)

    step("Now DEX receives balances stream from the node 2 and cancels Alice's order")
    dex1.api.waitForOrderStatus(aliceBuyOrder, Status.Cancelled)

    step("Bob places order that requires some amount of USD, DEX receives balances stream from the node 2")
    dex1.api.place(bobBuyOrder)
    dex1.api.waitForOrderStatus(bobBuyOrder, Status.Accepted)

    step("Up node 1")
    wavesNode1.start()

    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)
    wavesNode1.api.waitForTransaction(alice2BobTransferTx)
    wavesNode1.api.waitForHeight(wavesNode2.api.currentHeight)

    step(s"Stop node 2 and perform USD transfer from Bob to Alice")
    wavesNode2.stopWithoutRemove()

    broadcastAndAwait(wavesNode1.api, bob2AliceTransferTx)
    usdBalancesShouldBe(wavesNode1.api, defaultAssetQuantity, 0)

    step("Now DEX receives balances stream from the node 1 and cancels Bob's order")
    dex1.api.waitForOrderStatus(bobBuyOrder, Status.Cancelled)
  }

  "DEXClient should correctly handle gRPC errors" in {
    val randomKP = mkKeyPair("random")
    val order = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    // This request creates an actor. Otherwise the further check will fail by another reason:
    //   an actor will stuck during the initialization.
    dex1.api.getTradableBalanceByAssetPairAndAddress(randomKP, wavesUsdPair) should matchTo(Map.empty[Asset, Long])

    wavesNode1.disconnectFromNetwork()

    dex1.tryApi.place(order) should failWith(
      WavesNodeConnectionBroken.code,
      "Waves Node is unavailable, please retry later or contact with the administrator"
    )

    wavesNode1.connectToNetwork()

    dex1.api.waitForOrderPlacement(order)
    dex1.api.waitForOrderStatus(order, Status.Accepted)
    dex1.api.getTradableBalanceByAssetPairAndAddress(randomKP, wavesUsdPair) should matchTo(Map.empty[Asset, Long])
  }

  private def usdBalancesShouldBe(wavesNodeApi: NodeApi[Id], expectedAliceBalance: Long, expectedBobBalance: Long): Unit = {
    withClue("alice:")(wavesNodeApi.balance(alice, usd) shouldBe expectedAliceBalance)
    withClue("bob:")(wavesNodeApi.balance(bob, usd) shouldBe expectedBobBalance)
  }

}
