package com.wavesplatform.it.sync

import cats.Id
import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.NodeApi
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.fp
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.OrderStatus
import com.wavesplatform.transaction.assets.exchange.OrderType
import monix.eval.Coeval

import scala.util.Try

class DEXClientFaultToleranceTestSuite extends MatcherSuiteBase {

  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    s"""waves.dex.waves-node-grpc {
      |  host = $wavesNodesDomain
      |  port = 6887
      |}""".stripMargin
  )

  private val wavesNode2Container: Coeval[WavesNodeContainer] = Coeval.evalOnce { createWavesNode("waves-2") }

  private def wavesNode2Api: NodeApi[Id] = {
    val apiAddress = dockerClient.getExternalSocketAddress(wavesNode2Container(), wavesNode2Container().restApiPort)
    fp.sync(NodeApi[Try]("integration-test-rest-api", apiAddress))
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
  }

  "DEXClient should works correctly despite of the short connection losses" in {
    val aliceBuyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNode1Api.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNode1Api.balance(bob, usd), asset = usd)

    markup("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1")
    dex1Api.place(aliceBuyOrder)
    dex1Api.waitForOrderStatus(aliceBuyOrder, OrderStatus.Accepted)

    markup(s"Disconnect DEX from the network and perform USD transfer from Alice to Bob")
    dockerClient.disconnectFromNetwork(dex1Container)

    broadcastAndAwait(wavesNode1Api, alice2BobTransferTx)
    usdBalancesShouldBe(wavesNode1Api, 0, defaultAssetQuantity)

    Thread.sleep(2000)

    markup("Connect DEX back to the network, DEX should know about transfer and cancel Alice's order")
    dockerClient.connectToNetwork(dex1Container, None)
    dex1Api.waitForOrderStatus(aliceBuyOrder, OrderStatus.Cancelled)

    withClue("Cleanup") {
      broadcastAndAwait(wavesNode1Api, bob2AliceTransferTx)
    }
  }

  "DEXClient should switch nodes if connection to one of them was lost due to node shutdown" in {

    // also works for the cases when nodes are disconnected from the network (not stopped),
    // in these cases some delays after disconnections are required

    val aliceBuyOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 300)
    val bobBuyOrder   = mkOrder(bob, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNode2Api.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNode1Api.balance(bob, usd), asset = usd)

    markup("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1")
    dex1Api.place(aliceBuyOrder)
    dex1Api.waitForOrderStatus(aliceBuyOrder, OrderStatus.Accepted)

    markup("Up node 2")
    dockerClient.start(wavesNode2Container)
    wavesNode2Api.waitReady // waitReady(wavesNode2ApiAddress)

    wavesNode2Api.connect(wavesNode1NetworkApiAddress)
    wavesNode2Api.waitForConnectedPeer(wavesNode1NetworkApiAddress)

    wavesNode2Api.waitForTransaction(IssueUsdTx)

    markup(s"Stop node 1 and perform USD transfer from Alice to Bob")
    dockerClient.stop(wavesNode1Container)

    broadcastAndAwait(wavesNode2Api, alice2BobTransferTx)
    usdBalancesShouldBe(wavesNode2Api, 0, defaultAssetQuantity)

    markup("Now DEX receives balances stream from the node 2 and cancels Alice's order")
    dex1Api.waitForOrderStatus(aliceBuyOrder, OrderStatus.Cancelled)

    markup("Bob places order that requires some amount of USD, DEX receives balances stream from the node 2")
    dex1Api.place(bobBuyOrder)
    dex1Api.waitForOrderStatus(bobBuyOrder, OrderStatus.Accepted)

    markup("Up node 1")
    dockerClient.start(wavesNode1Container)
    wavesNode1Api.waitReady
    wavesNode2Api.connect(wavesNode1NetworkApiAddress)
    wavesNode2Api.waitForConnectedPeer(wavesNode1NetworkApiAddress)
    wavesNode1Api.waitForTransaction(alice2BobTransferTx)

    markup(s"Stop node 2 and perform USD transfer from Bob to Alice")
    dockerClient.stop(wavesNode2Container)

    broadcastAndAwait(wavesNode1Api, bob2AliceTransferTx)
    usdBalancesShouldBe(wavesNode1Api, defaultAssetQuantity, 0)

    markup("Now DEX receives balances stream from the node 1 and cancels Bob's order")
    dex1Api.waitForOrderStatus(bobBuyOrder, OrderStatus.Cancelled)

    dockerClient.stop(wavesNode2Container)
  }

  private def usdBalancesShouldBe(wavesNodeApi: NodeApi[Id], expectedAliceBalance: Long, expectedBobBalance: Long): Unit = {
    withClue("alice:")(wavesNodeApi.balance(alice, usd) shouldBe expectedAliceBalance)
    withClue("bob:")(wavesNodeApi.balance(bob, usd) shouldBe expectedBobBalance)
  }
}
