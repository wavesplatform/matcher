package com.wavesplatform.it.sync

import java.net.InetSocketAddress

import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.{NodeApi, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.docker.{DexContainer, Docker, WavesNodeContainer}
import com.wavesplatform.it.util._
import com.wavesplatform.it.{NewMatcherSuiteBase, fp}
import com.wavesplatform.transaction.assets.exchange.OrderType
import monix.eval.Coeval

import scala.util.Try

class DEXClientFaultToleranceTestSuite extends NewMatcherSuiteBase {

  override val dex1Container: Coeval[DexContainer] = Coeval.evalOnce {
    val config =
      DexTestConfig.updatedMatcherConfig
        .withFallback {
          ConfigFactory
            .parseString(s"""waves.dex.waves-node-grpc {
                          |  host = ${Docker.wavesNodesNetworkAlias}
                          |  port = 6887
                          |}""".stripMargin)
        }
        .withFallback(dex1Config)
        .resolve

    dockerClient().createDex("dex-1", config)
  }

  def wavesNode2Config: Config = DexTestConfig.containerConfig("waves-2")

  val wavesNode2Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-2", wavesNode1Config.resolve)
  }

  def wavesNode2Api: NodeApi[cats.Id] = {
    def apiAddress = dockerClient().getExternalSocketAddress(wavesNode2Container(), wavesNode2Config.getInt("waves.rest-api.port"))
    fp.sync(NodeApi[Try]("integration-test-rest-api", apiAddress))
  }

  def wavesNode2NetworkApiAddress: InetSocketAddress = {
    dockerClient().getInternalSocketAddress(wavesNode2Container(), wavesNode2Config.getInt("waves.network.port"))
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueUsdTx)
    System.setProperty("networkaddress.cache.ttl", "0")
  }

  "DEXClient should switch node if connection to it was lost" in {

    val aliceBuyOrder = mkOrder(alice, matcher, wavesUsdPair, OrderType.BUY, 1.waves, 300)
    val bobBuyOrder   = mkOrder(bob, matcher, wavesUsdPair, OrderType.BUY, 1.waves, 300)

    lazy val alice2BobTransferTx = mkTransfer(alice, bob, amount = wavesNode2Api.balance(alice, usd), asset = usd)
    lazy val bob2AliceTransferTx = mkTransfer(bob, alice, amount = wavesNode1Api.balance(bob, usd), asset = usd)

    withClue("Alice places order that requires some amount of USD, DEX receives balances stream from the node 1\n") {
      dex1Api.place(aliceBuyOrder)
      dex1Api.waitForOrderStatus(aliceBuyOrder, OrderStatus.Accepted)
    }

    withClue("Up node 2\n") {
      dockerClient().start(wavesNode2Container)
      wavesNode2Api.waitReady
      wavesNode2Api.connect(wavesNode1NetworkApiAddress)
      wavesNode2Api.waitForConnectedPeer(wavesNode1NetworkApiAddress)
      wavesNode2Api.waitForTransaction(IssueUsdTx)
    }

    withClue(s"Disconnect node 1 from the network and perform USD transfer from Alice to Bob\n") {
      dockerClient().disconnectFromNetwork(wavesNode1Container)
//      Thread.sleep(5000)
//      dockerClient().stop(wavesNode1Container)
      wavesNode2Api.broadcast(alice2BobTransferTx)
      wavesNode2Api.waitForTransaction(alice2BobTransferTx)
      wavesNode2Api.balance(alice, usd) shouldBe 0
      wavesNode2Api.balance(bob, usd) shouldBe defaultAssetQuantity
    }

    withClue("Now DEX receives balances stream from the node 2 and cancels Alice's order\n") {
      dex1Api.waitForOrderStatus(aliceBuyOrder, OrderStatus.Cancelled)
    }

    withClue("Bob places order that requires some amount of USD, DEX receives balances stream from the node 2\n") {
      dex1Api.place(bobBuyOrder)
      dex1Api.waitForOrderStatus(bobBuyOrder, OrderStatus.Accepted)
    }

    withClue("Connect the node 1 back to the network\n") {
      dockerClient().connectToNetwork(wavesNode1Container, Some(Docker.wavesNodesNetworkAlias))
//      dockerClient().start(wavesNode1Container)
      wavesNode1Api.waitReady
//      wavesNode1Api.connect(wavesNode2NetworkApiAddress)
//      wavesNode1Api.waitForConnectedPeer(wavesNode2NetworkApiAddress)
      wavesNode1Api.waitForTransaction(alice2BobTransferTx)
    }

    withClue(s"Stop node 2 and perform USD transfer from Bob to Alice\n") {
//      dockerClient().stop(wavesNode2Container)
      dockerClient().disconnectFromNetwork(wavesNode2Container)
//      Thread.sleep(20000)
      wavesNode1Api.broadcast(bob2AliceTransferTx)
      wavesNode1Api.waitForTransaction(bob2AliceTransferTx)
      wavesNode1Api.balance(alice, usd) shouldBe defaultAssetQuantity
      wavesNode1Api.balance(bob, usd) shouldBe 0
    }

    withClue("Now DEX receives balances stream from the node 1 and cancels Bob's order\n") {
      dex1Api.waitForOrderStatus(bobBuyOrder, OrderStatus.Cancelled)
    }
  }
}
