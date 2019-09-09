package com.wavesplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi.{sync => _}
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}

class DisableProducerTestSuite extends NewMatcherSuiteBase {
  override protected val suiteInitialDexConfig: Config = ConfigFactory.parseString(
    """waves.dex.events-queue {
      |  local.enable-storing  = no
      |  kafka.producer.enable = no
      |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(IssueEthTx)
  }

  "Check no commands are written to queue" - {
    "check assets's balances" in {
      wavesNode1Api.balance(alice, EthAsset) shouldBe IssueEthTx.quantity
      wavesNode1Api.balance(matcher, EthAsset) shouldBe 0L
    }

    "place an order and wait some time" in {
      def test(order: Order): Unit = dex1Api.tryPlace(order) should failWith(528) // FeatureDisabled

      List(
        mkOrder(alice, ethWavesPair, OrderType.SELL, 500, 2.waves * Order.PriceConstant),
        mkOrder(alice, ethWavesPair, OrderType.BUY, 500, 2.waves * Order.PriceConstant)
      ).foreach(test)

      Thread.sleep(5000)

      dex1Api.currentOffset should be(-1)
      dex1Api.lastOffset should be(-1)
    }

    "Commands aren't written to queue after restart" in {
      dockerClient().stop(dex1Container())
      dockerClient().start(dex1Container())
      dex1Api.waitReady

      dex1Api.currentOffset should be(-1)
      dex1Api.lastOffset should be(-1)
    }
  }
}
