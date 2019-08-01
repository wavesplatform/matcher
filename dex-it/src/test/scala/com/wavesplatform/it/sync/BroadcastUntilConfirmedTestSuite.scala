package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderV1}

import scala.concurrent.duration.DurationInt

class BroadcastUntilConfirmedTestSuite extends NewMatcherSuiteBase {

  override protected val dexNodeConfig: Config =
    ConfigFactory
      .parseString(s"""waves {
                      |  miner.enable = no
                      |  dex.exchange-transaction-broadcast {
                      |    broadcast-until-confirmed = yes
                      |    interval = 20s
                      |  }
                      |}""".stripMargin)
      .withFallback(super.dexNodeConfig)

  "BroadcastUntilConfirmed" in {
    markup("Issue an asset")
    wavesApi.broadcast(IssueEthTx)
    val pair = AssetPair(IssuedAsset(IssueEthTx.id()), Waves)
    wavesApi.waitForTransaction(IssueEthTx.id())
    wavesApi.waitForHeightArise()

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
    dockerClient().disconnectFromNetwork(wavesContainer())

    markup("Place orders, those should match")
    dexApi.place(alicePlace)
    dexApi.place(bobPlace)
    dexApi.waitForOrderStatus(alicePlace.id(), "Filled")
    val exchangeTxId = dexApi.waitForTransactionsByOrder(alicePlace.id(), 1).head.id()

    markup("Start miners and wait until it receives the transaction")
    dockerClient().connectToNetwork(wavesContainer())
    wavesApi.waitForTransaction(exchangeTxId)
  }
}
