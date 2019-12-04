package com.wavesplatform.it.sync

import cats.Id
import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.it.api.NodeApi
import com.wavesplatform.dex.it.cache.CachedData
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.dex.it.fp
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.dex.OrderStatus
import com.wavesplatform.transaction.assets.exchange.OrderType
import monix.eval.Coeval

import scala.util.Try

class BroadcastUntilConfirmedTestSuite extends MatcherSuiteBase {

  override protected def suiteInitialDexConfig: Config =
    ConfigFactory
      .parseString(s"""waves.dex.exchange-transaction-broadcast {
                    |  broadcast-until-confirmed = yes
                    |  interval = 20s
                    |}""".stripMargin)

  // Validator node
  protected val wavesNode2Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    createWavesNode("waves-2", suiteInitialConfig = ConfigFactory.parseString("waves.miner.enable = no").withFallback(suiteInitialWavesNodeConfig))
  }

  protected val refreshableWavesNode2ApiAddress = CachedData {
    dockerClient.getExternalSocketAddress(wavesNode2Container(), wavesNode2Container().restApiPort)
  }

  protected def wavesNode2Api: NodeApi[Id] = fp.sync(NodeApi[Try]("integration-test-rest-api", refreshableWavesNode2ApiAddress.get()))

  private val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 100000L, 80000L)
  private val bobOrder   = mkOrder(bob, ethWavesPair, OrderType.BUY, 200000L, 100000L)

  "BroadcastUntilConfirmed" in {
    markup("Disconnect a miner node from the network")
    dockerClient.disconnectFromNetwork(wavesNode1Container())

    markup("Place orders, those should match")
    dex1Api.place(aliceOrder)
    dex1Api.place(bobOrder)
    dex1Api.waitForOrderStatus(aliceOrder, OrderStatus.Filled)

    markup("Wait for a transaction")
    val exchangeTxId = dex1Api.waitForTransactionsByOrder(aliceOrder, 1).head.id()

    markup("Connect the miner node to the network")
    dockerClient.connectToNetwork(wavesNode1Container())

    markup("Wait until it receives the transaction")
    wavesNode2Api.waitForTransaction(exchangeTxId)
  }

  override protected def beforeAll(): Unit = {
    dockerClient.start(wavesNode1Container)
    dockerClient.start(wavesNode2Container)

    wavesNode1Api.waitReady

    startAndWait(dex1Container(), dex1Api)

    wavesNode2Api.waitReady
    wavesNode2Api.connect(wavesNode1NetworkApiAddress)
    wavesNode2Api.waitForConnectedPeer(wavesNode1NetworkApiAddress)

    broadcastAndAwait(IssueEthTx)
  }

  override protected def invalidateCaches(): Unit = {
    super.invalidateCaches()
    refreshableWavesNode2ApiAddress.invalidate()
  }
}
