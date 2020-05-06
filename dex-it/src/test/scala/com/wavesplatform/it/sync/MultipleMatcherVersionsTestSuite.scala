package com.wavesplatform.it.sync

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType.BUY
import com.wavesplatform.dex.it.api.HasKafka
import com.wavesplatform.dex.it.api.responses.dex.OrderStatus
import com.wavesplatform.dex.it.docker.{DexContainer, WavesNodeContainer}
import com.wavesplatform.it.MatcherSuiteBase

class MultipleMatcherVersionsTestSuite extends MatcherSuiteBase with HasKafka {

  val dexPrevTag  = Option(System.getenv("DEX_MULTIPLE_VERSIONS_PREVIOUS_TAG")).getOrElse("latest")
  val nodePrevTag = Option(System.getenv("NODE_MULTIPLE_VERSIONS_PREVIOUS_TAG")).getOrElse("latest")

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""waves.dex.price-assets = [ "$UsdId", "WAVES" ]""".stripMargin)

  override protected lazy val dexRunConfig = dexKafkaConfig()

  lazy val wavesNode2: WavesNodeContainer = createWavesNode("waves-2", tag = nodePrevTag: String)

  def dexPrevConfig: Config = {
    ConfigFactory.parseString(s"""waves.dex {
                                   |  price-assets = [ "$UsdId", "WAVES" ]
                                   |  waves-blockchain-client.grpc.target = "${wavesNode2.networkAddress.getHostName}:6887"
                                   |}""".stripMargin)
  }

  protected lazy val dex2: DexContainer = createDex("dex-2", suiteInitialConfig = dexPrevConfig, tag = dexPrevTag)

  override protected def beforeAll(): Unit = {
    kafka.start()
    wavesNode1.start()
    wavesNode2.start()
    wavesNode2.api.connect(wavesNode1.networkAddress)
    wavesNode2.api.waitForConnectedPeer(wavesNode1.networkAddress)
    wavesNode2.api.waitForHeight(wavesNode1.api.currentHeight)
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  "test 1" in {
    val o = mkOrder(alice, wavesUsdPair, BUY, 10.waves, 1.usd)
    placeAndAwaitAtDex(o)
    dex1.api.waitForOrderStatus(o, OrderStatus.Accepted)

    dex1.disconnectFromNetwork()

    dex2.start()
    dex2.api.waitForOrderStatus(o, OrderStatus.Accepted)
  }

  Seq(1, 2, 3).foreach { v =>

    "sadasdas $v" in {
      
    }

  }

}
