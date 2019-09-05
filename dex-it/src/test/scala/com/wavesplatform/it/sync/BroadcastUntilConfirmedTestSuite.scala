package com.wavesplatform.it.sync

import java.net.InetSocketAddress

import cats.Id
import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.api.{HasWaitReady, NodeApi, OrderStatus}
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.docker.{DockerContainer, WavesNodeContainer}
import com.wavesplatform.it.{NewMatcherSuiteBase, fp}
import com.wavesplatform.transaction.assets.exchange.OrderType
import monix.eval.Coeval

import scala.util.Try

class BroadcastUntilConfirmedTestSuite extends NewMatcherSuiteBase {

  // Validator node
  protected def getWavesNode2ApiAddress: InetSocketAddress =
    dockerClient().getExternalSocketAddress(wavesNode2Container(), wavesNode2Config.getInt("waves.rest-api.port"))
  protected def wavesNode2Api: NodeApi[Id] = fp.sync(NodeApi[Try]("integration-test-rest-api", getWavesNode2ApiAddress))
  protected def wavesNode2Config: Config   = DexTestConfig.containerConfig("waves-2")
  protected val wavesNode2Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-2", wavesNode2Config.resolve())
  }

  // DEX connects to a waves validator node
  override protected def dex1NodeContainer = wavesNode2Container()
  override protected val dex1Config: Config =
    ConfigFactory
      .parseString(s"""waves {
                      |  miner.enable = no
                      |  dex.exchange-transaction-broadcast {
                      |    broadcast-until-confirmed = yes
                      |    interval = 20s
                      |  }
                      |}""".stripMargin)
      .withFallback(super.dex1Config)

  // Must start before DEX, otherwise DEX will fail to start.
  // DEX needs to know activated features to know which order versions should be enabled.
  override protected def allContainers: List[DockerContainer] = wavesNode2Container() :: super.allContainers
  override protected def allApis: List[HasWaitReady[Id]]      = wavesNode2Api :: super.allApis

  private val aliceOrder = mkOrder(alice, matcher, ethWavesPair, OrderType.SELL, 100000L, 80000L)
  private val bobOrder   = mkOrder(bob, matcher, ethWavesPair, OrderType.BUY, 200000L, 100000L)

  "BroadcastUntilConfirmed" in {
    markup("Disconnect a miner node from the network")
    dockerClient().disconnectFromNetwork(wavesNode1Container())

    markup("Place orders, those should match")
    dex1Api.place(aliceOrder)
    dex1Api.place(bobOrder)
    dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

    markup("Wait for a transaction")
    val exchangeTxId = dex1Api.waitForTransactionsByOrder(aliceOrder.id(), 1).head.id()

    markup("Connect the miner node to the network")
    dockerClient().connectToNetwork(wavesNode1Container())

    markup("Wait until it receives the transaction")
    wavesNode2Api.waitForTransaction(exchangeTxId)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    wavesNode2Api.connect(wavesNode1NetworkApiAddress)
    wavesNode2Api.waitForConnectedPeer(wavesNode1NetworkApiAddress)
    broadcastAndAwait(IssueEthTx)
  }
}
