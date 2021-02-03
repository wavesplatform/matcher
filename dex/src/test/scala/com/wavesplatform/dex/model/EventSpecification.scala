package com.wavesplatform.dex.model

import cats.syntax.option._
import com.wavesplatform.dex.MatcherSpecBase
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EventSpecification extends AnyFreeSpec with Matchers with MatcherSpecBase {

  private val btcEthPair = AssetPair(btc, eth)

  private val alicePk = KeyPair("alice".getBytes("utf-8"))
  private val bobPk = KeyPair("bob".getBytes("utf-8"))

  "Proper rounding scenario 1" in {
    val counter = sell(wavesBtcPair, 840340L, 0.00000238, matcherFee = Some(300000L))
    val submitted = buy(wavesBtcPair, 425532L, 0.00000238, matcherFee = Some(300000L))
    val exec = mkOrderExecutedRaw(submitted, counter)
    exec.executedAmount shouldBe 420169L
    exec.counterRemainingAmount shouldBe 420171L
    exec.counterRemainingAmount shouldBe counter.amount - exec.executedAmount

    exec.counterRemainingFee shouldBe 150001L

    exec.submittedRemainingAmount shouldBe 5363L
    exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount

    exec.submittedRemainingFee shouldBe 3781L
  }

  "Remaining fee and amount checks" in {
    val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(2000L))
    val submitted = buy(wavesBtcPair, 120000000, 0.00085, matcherFee = Some(1000L))

    val exec = mkOrderExecutedRaw(submitted, counter)
    exec.submittedRemainingAmount shouldBe 20000000L
    exec.submittedRemainingFee shouldBe 167L
  }

  "Reserved balance should empty after full rounded execution" in {
    val counter = buy(btcEthPair, 923431000L, 0.00031887, matcherFee = Some(300000), sender = Some(alicePk))
    val submitted = sell(btcEthPair, 223345000L, 0.00031887, matcherFee = Some(300000), sender = Some(bobPk))

    val exec = mkOrderExecutedRaw(submitted, counter)
    exec.executedAmount shouldBe 223344937L
  }

  "counterExecutedSpending - Expected values when" - {
    "same spent and fee assets" in {
      val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(2000L), sender = alicePk.some)
      val submitted = buy(wavesBtcPair, 120000000, 0.00085, sender = alicePk.some, matcherFee = Some(1000L))
      val exec = mkOrderExecutedRaw(submitted, counter)
      exec.counterExecutedSpending should matchTo(Map[Asset, Long](Waves -> 100002000))
    }

    "different spent and fee assets" in {
      val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(2000L), feeAsset = eth, sender = alicePk.some, version = 3)
      val submitted = buy(wavesBtcPair, 120000000, 0.00085, matcherFee = Some(1000L), sender = alicePk.some)
      val exec = mkOrderExecutedRaw(submitted, counter)
      exec.counterExecutedSpending should matchTo(Map(
        Waves -> 100000000L,
        eth -> 2000L
      ))
    }

    "empty fee" in {
      val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(0L), feeAsset = eth, sender = alicePk.some, version = 3)
      val submitted = buy(wavesBtcPair, 120000000, 0.00085, matcherFee = Some(1000L), sender = alicePk.some)
      val exec = mkOrderExecutedRaw(submitted, counter)
      exec.counterExecutedSpending should matchTo(Map[Asset, Long](Waves -> 100000000))
    }
  }

  "submittedExecutedSpending - Expected values when" - {
    "same spent and fee assets" in {
      val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(2000L), sender = alicePk.some)
      val submitted = buy(wavesBtcPair, 120000000, 0.00085, matcherFee = Some(1000L), feeAsset = btc, sender = alicePk.some, version = 3)
      val exec = mkOrderExecutedRaw(submitted, counter)
      exec.submittedExecutedSpending should matchTo(Map[Asset, Long](btc -> 80833)) // = 100000000*0.0008 + 100000000/120000000*1000
    }

    "different spent and fee assets" in {
      val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(2000L), feeAsset = eth, sender = alicePk.some, version = 3)
      val submitted = buy(wavesBtcPair, 120000000, 0.00085, matcherFee = Some(1000L), sender = alicePk.some)
      val exec = mkOrderExecutedRaw(submitted, counter)
      exec.submittedExecutedSpending should matchTo(Map(
        Waves -> 833L, // 100000000/120000000*1000
        btc -> 80000 // 100000000*0.0008
      ))
    }

    "empty fee" in {
      val counter = sell(wavesBtcPair, 100000000, 0.0008, matcherFee = Some(2000L), sender = alicePk.some)
      val submitted = buy(wavesBtcPair, 120000000, 0.00085, matcherFee = Some(0L), feeAsset = eth, sender = alicePk.some, version = 3)
      val exec = mkOrderExecutedRaw(submitted, counter)
      exec.submittedExecutedSpending should matchTo(Map[Asset, Long](btc -> 80000)) // = 100000000*0.0008
    }

  }
}
