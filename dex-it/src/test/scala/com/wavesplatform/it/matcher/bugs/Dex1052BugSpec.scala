package com.wavesplatform.it.matcher.bugs

import cats.syntax.option._
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.it.MatcherSuiteBase

// Wrong balances
class Dex1052BugSpec extends MatcherSuiteBase {

  private val storedBlocks = 2

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  price-assets = [ "$BtcId", "$UsdId", "WAVES" ]
         |  waves-blockchain-client.combined-client-settings {
         |    max-rollback-height = $storedBlocks
         |    max-cached-latest-block-updates = $storedBlocks
         |  }
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = wavesNode1.start()

  "DEX-1052" in {
    // Believe me, we shouldn't batch them here
    List(IssueUsdTx, IssueBtcTx, IssueEthTx, mkTransfer(alice, bob, 1.waves, Waves)).foreach(broadcastAndAwait(_))
    wavesNode1.api.waitForHeight(wavesNode1.api.currentHeight + storedBlocks)

    val aliceWavesBalanceBefore = wavesNode1.api.balance(alice, Waves)
    val bobWavesBalanceBefore = wavesNode1.api.balance(bob, Waves)

    dex1.start()

    val transferAmount = 100.usd
    wavesNode1.api.broadcast(mkTransfer(alice, bob, transferAmount, usd))

    withClue("alice: ") {
      dex1.api.getTradableBalance(alice, wavesUsdPair) should matchTo(Map[Asset, Long](
        Waves -> (aliceWavesBalanceBefore - minFee),
        usd -> (IssueUsdTx.quantity() - transferAmount)
      ))
      dex1.api.getTradableBalance(alice, ethWavesPair).get(eth) should matchTo(IssueEthTx.quantity().some)
      dex1.api.getTradableBalance(alice, wavesBtcPair).get(btc) should matchTo(none[Long])
    }

    withClue("bob: ") {
      dex1.api.getTradableBalance(bob, wavesUsdPair).get(usd) should (
        matchTo(none[Long]) or // not confirmed
        matchTo(transferAmount.some) // confirmed
      )
      dex1.api.getTradableBalance(bob, ethWavesPair).get(eth) should matchTo(none[Long])
      dex1.api.getTradableBalance(bob, wavesBtcPair) should matchTo(Map[Asset, Long](
        Waves -> bobWavesBalanceBefore,
        btc -> IssueBtcTx.quantity()
      ))
    }

    wavesNode1.api.waitForHeightArise()
    dex1.api.getTradableBalance(bob, wavesUsdPair).get(usd) should matchTo(transferAmount.some)
  }

}
