package com.wavesplatform.it.sync

import java.net.InetSocketAddress

import cats.Id
import cats.instances.try_._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.NodeApi
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.docker.{DockerContainer, WavesNodeContainer}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderV1}
import monix.eval.Coeval

import scala.concurrent.duration.DurationInt
import scala.util.Try

class BroadcastUntilConfirmedTestSuite extends NewMatcherSuiteBase {

  // Validator node
  protected def getWavesNode2ApiAddress: InetSocketAddress =
    dockerClient().getExternalSocketAddress(wavesNode1Container(), wavesNode1Config.getInt("waves.rest-api.port"))
  protected def wavesNode2Api: NodeApi[Id] = NodeApi.unWrapped(NodeApi[Try]("integration-test-rest-api", getWavesNode2ApiAddress))
  protected def wavesNode2Config: Config   = DexTestConfig.containerConfig("waves-2")
  protected val wavesNode2Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode("waves-2", wavesNode1Config.resolve())
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

  "BroadcastUntilConfirmed" in {
    markup("Issue an asset")
    wavesNode1Api.broadcast(IssueEthTx)
    val pair = AssetPair(IssuedAsset(IssueEthTx.id()), Waves)
    wavesNode1Api.waitForTransaction(IssueEthTx.id())

    markup("Prepare orders")
    val now = System.currentTimeMillis()
    val alicePlace = OrderV1.sell(
      sender = alice,
      matcher = matcher,
      pair = pair,
      amount = 100000L,
      price = 80000L,
      timestamp = now,
      expiration = now + 1.day.toMillis,
      matcherFee = 300000L
    )

    val bobPlace = OrderV1.buy(
      sender = bob,
      matcher = matcher,
      pair = pair,
      amount = 200000L,
      price = 100000L,
      timestamp = now,
      expiration = now + 1.day.toMillis,
      matcherFee = 300000L
    )

    markup("Shutdown a dependent NODE container")
    dockerClient().disconnectFromNetwork(dex1NodeContainer)

    markup("Place orders, those should match")
    dex1Api.place(alicePlace)
    dex1Api.place(bobPlace)
    dex1Api.waitForOrderStatus(alicePlace.id(), "Filled")
    val exchangeTxId = dex1Api.waitForTransactionsByOrder(alicePlace.id(), 1).head.id()

    markup("Start miners and wait until it receives the transaction")
    dockerClient().connectToNetwork(dex1NodeContainer)
    wavesNode2Api.waitForTransaction(exchangeTxId)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    wavesNode1Api.waitReady
    wavesNode2Api.waitReady
    wavesNode2Api.connect(wavesNode1NetworkApiAddress)
  }
}
