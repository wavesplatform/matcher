package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.it.MatcherSuiteBase

class BroadcastUntilConfirmedTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory
      .parseString(s"""waves.dex.exchange-transaction-broadcast {
                      |  broadcast-until-confirmed = yes
                      |  interval = 10s
                      |}""".stripMargin)

  // Validator node
  protected lazy val wavesNode2: WavesNodeContainer = {
    createWavesNode("waves-2", suiteInitialConfig = ConfigFactory.parseString("waves.miner.enable = no") withFallback wavesNodeInitialSuiteConfig)
  }

  private val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 100000L, 80000L)
  private val bobOrder   = mkOrder(bob, ethWavesPair, OrderType.BUY, 200000L, 100000L)

  "BroadcastUntilConfirmed" in {
    markup("Disconnect a miner node from the network")
    wavesNode1.disconnectFromNetwork()

    markup("Place orders, those should match")
    eventually { dex1.api.tryPlace(aliceOrder) shouldBe Symbol("right") }

    dex1.api.place(bobOrder)
    dex1.api.waitForOrderStatus(aliceOrder, Status.Filled)

    markup("Wait for a transaction")
    val exchangeTxId = dex1.api.waitForTransactionsByOrder(aliceOrder, 1).head.getId

    markup("Connect the miner node to the network")
    wavesNode1.connectToNetwork()

    markup("Wait until it receives the transaction")
    wavesNode2.api.waitForTransaction(exchangeTxId)
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    wavesNode2.start()

    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)

    dex1.start()

    broadcastAndAwait(IssueEthTx)
    wavesNode2.api.waitForTransaction(IssueEthTx)
  }
}
