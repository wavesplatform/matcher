package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.it.docker.DexContainer

final class Test extends OrderFeeBaseTestSuite {

  "Test" - {
    "test" in {
      //6887 me
      //6881 bu

      println("*****RB1=" + dex1.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair))
      println("*****RB2=" + dex2.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair))

      dex1.switchPort(6881, block = true)
      Thread.sleep(3000L)

      broadcastAndAwait(mkTransfer(alice, bob, 1.waves, Waves))
      Thread.sleep(10000L)

      println("*****RB3=" + dex1.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair))
      println("*****RB4=" + dex2.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair))
      Thread.sleep(3000L)

      dex1.switchPort(6881, block = false)
      Thread.sleep(10000L)

      println("*****RB5=" + dex1.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair))
      println("*****RB6=" + dex2.api.getTradableBalanceByAssetPairAndAddress(bob, wavesBtcPair))
    }
  }

  override protected val dexInitialSuiteConfig: Config = ConfigFactory
    .parseString(
      s"""waves.dex {
         |  price-assets = [ "$BtcId", "WAVES" ]
         |}""".stripMargin
    )

  private lazy val dex2: DexContainer = createDex("dex-2")

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx)
    dex1.start()
    dex2.start()
  }

}
