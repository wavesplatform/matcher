package com.wavesplatform.it.sync

import java.net.InetSocketAddress

import cats.Id
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.NodeApi
import com.wavesplatform.it.config.DexTestConfig
import com.wavesplatform.it.config.DexTestConfig._
import cats.Id
import cats.instances.try_._
import com.wavesplatform.it.docker.WavesNodeContainer
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderV1}
import monix.eval.Coeval

import scala.concurrent.duration.DurationInt
import scala.util.Try

class BroadcastUntilConfirmedTestSuite extends NewMatcherSuiteBase {

  // TODO naming

  // The default Waves NODE container is not a miner
  override protected def wavesNodeName = "waves-6"

  // But a second one is the miner
  protected def getWavesNode2ApiAddress: InetSocketAddress =
    dockerClient().getExternalSocketAddress(wavesNodeContainer(), wavesNodeConfig.getInt("waves.rest-api.port"))
  protected def wavesNode2Api: NodeApi[Id] = NodeApi.unWrapped(NodeApi[Try]("integration-test-rest-api", getWavesNode2ApiAddress))
  protected def wavesNode2Name             = "waves-5"
  protected def wavesNode2Config: Config   = DexTestConfig.containerConfig(wavesNode2Name)
  protected val wavesNode2Container: Coeval[WavesNodeContainer] = Coeval.evalOnce {
    dockerClient().createWavesNode(wavesNode2Name, wavesNodeConfig.resolve())
  }

  override protected val dexConfig: Config =
    ConfigFactory
      .parseString(s"""waves {
                      |  miner.enable = no
                      |  dex.exchange-transaction-broadcast {
                      |    broadcast-until-confirmed = yes
                      |    interval = 20s
                      |  }
                      |}""".stripMargin)
      .withFallback(super.dexConfig)

  "BroadcastUntilConfirmed" in {
    markup("Issue an asset")
    wavesNodeApi.broadcast(IssueEthTx)
    val pair = AssetPair(IssuedAsset(IssueEthTx.id()), Waves)
    wavesNodeApi.waitForTransaction(IssueEthTx.id())
    wavesNodeApi.waitForHeightArise()

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

    markup("Shutdown miners")
    dockerClient().disconnectFromNetwork(wavesNode2Container())

    markup("Place orders, those should match")
    dexApi.place(alicePlace)
    dexApi.place(bobPlace)
    dexApi.waitForOrderStatus(alicePlace.id(), "Filled")
    val exchangeTxId = dexApi.waitForTransactionsByOrder(alicePlace.id(), 1).head.id()

    markup("Start miners and wait until it receives the transaction")
    dockerClient().connectToNetwork(wavesNode2Container())
    wavesNode2Api.waitForTransaction(exchangeTxId)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    dockerClient().start(wavesNode2Container())
    wavesNode2Api.connect(getWavesNodeNetworkApiAddress)
  }

  override protected def afterAll(): Unit = {
    dockerClient().stop(wavesNode2Container())
    super.afterAll()
  }
}
