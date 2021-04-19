package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.docker.WavesNodeContainer
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.EitherValues

class ReBroadcastUntilConfirmedTestSuite extends MatcherSuiteBase with EitherValues {

  // Validator node
  protected lazy val wavesNode2: WavesNodeContainer =
    createWavesNode("waves-2", suiteInitialConfig = ConfigFactory.parseString("waves.miner.enable = no") withFallback wavesNodeInitialSuiteConfig)

  override protected def dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(s"""waves.dex.exchange-transaction-broadcast.interval = 10s
                    | waves.dex.waves-blockchain-client.blockchain-updates-grpc.target = "${wavesNode2.internalIp}:6887"
                    | waves.dex.waves-blockchain-client.grpc.target = "${wavesNode2.internalIp}:6887"
    """)

  private val aliceOrder = mkOrder(alice, ethWavesPair, OrderType.SELL, 100000L, 80000L)
  private val bobOrder = mkOrder(bob, ethWavesPair, OrderType.BUY, 200000L, 100000L)

  "ReBroadcastUntilConfirmed" in {
    markup("Disconnect a miner node from the network")
    wavesNode1.disconnectFromNetwork()

    markup("Place orders, those should match")
    eventually(dex1.api.place(aliceOrder))
    dex1.api.place(bobOrder)

    markup("Wait for a transaction")
    val exchangeTxId = dex1.api.waitForTransactionsByOrder(aliceOrder, 1).head.id()

    markup("Check that disconnected miner node didn't get a transaction")
    wavesNode2.tryApi.unconfirmedTransactionInfo(exchangeTxId).isRight shouldBe true

    markup("Connect the miner node to the network")
    wavesNode1.connectToNetwork()

    markup("Wait until it receives the transaction")
    wavesNode1.api.waitForTransaction(exchangeTxId)

    markup("Check that transaction is confirmed")
    wavesNode2.tryApi.transactionInfo(exchangeTxId).isRight shouldBe true
    wavesNode1.tryApi.transactionInfo(exchangeTxId).isRight shouldBe true
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
