package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.model.Normalization._
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.MatcherSuiteBase
import org.scalatest.prop.TableDrivenPropertyChecks

class MakerTakerFeeTestSuite extends MatcherSuiteBase with TableDrivenPropertyChecks {

  private val maker = bob
  private val taker = alice

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = dynamic
       |    dynamic {
       |      base-maker-fee = ${0.001.waves}
       |      base-taker-fee = ${0.005.waves}
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()

    broadcastAndAwait(IssueUsdTx, IssueEthTx)
    broadcastAndAwait(mkTransfer(alice, bob, 100.eth, eth))

    dex1.start()
    dex1.api.upsertRate(eth, 0.00567593)
  }

  "DEX with static non-default DynamicSettings" - {

    "should reject orders with insufficient fee" in {
      dex1.api.tryPlace(mkOrderDP(maker, wavesUsdPair, SELL, 1.waves, 3.00, 0.00499999.waves)) should failWith(
        9441542, // FeeNotEnough
        s"Required 0.005 WAVES as fee for this order, but given 0.00499999 WAVES"
      )

      dex1.api.tryPlace(mkOrderDP(maker, wavesUsdPair, SELL, 1.waves, 3.00, 0.00002837.eth, eth)) should failWith(
        9441542, // FeeNotEnough
        s"Required 0.00002838 $EthId as fee for this order, but given 0.00002837 $EthId"
      )
    }

    "should charge different fees for makers (SELL) and takers (BUY)" in {
      // format: off
      forAll(
        Table(
          ("M amt", "M fee", "M fee asset", "T amt", "T fee", "T fee asset", "M expected balance change", "T expected balance change", "is T market"),
          (1.waves,  0.005, Waves, 1.waves,  0.005, Waves, -1.001.waves,  0.995.waves, false), // symmetric
          (2.waves,  0.005, Waves, 10.waves, 0.005, Waves, -2.001.waves,  1.999.waves, false), // little maker - big taker
          (10.waves, 0.005, Waves, 2.waves,  0.005, Waves, -2.0002.waves, 1.995.waves, false), //    big maker - little taker
          (1.waves,  0.005, Waves, 1.waves,  0.005, Waves, -1.001.waves,  0.995.waves, true),  // symmetric, MARKET taker
          (2.waves,  0.005, Waves, 10.waves, 0.005, Waves, -2.001.waves,  1.999.waves, true),  // little maker - big MARKET taker
          (10.waves, 0.005, Waves, 2.waves,  0.005, Waves, -2.0002.waves, 1.995.waves, true),  //    big maker - little MARKET taker
          /** fee in ETH, 0.001.waves = 0.00000568.eth, 0.005.waves = 0.00002838.eth */
          (1.waves,  0.00002838, eth, 1.waves,  0.00002838, eth, -0.00000568.eth, -0.00002838.eth, false), // symmetric
          (2.waves,  0.00002838, eth, 10.waves, 0.00002838, eth, -0.00000568.eth, -0.00000567.eth, false), // little maker - big taker
          (10.waves, 0.00002838, eth, 2.waves,  0.00002838, eth, -0.00000113.eth, -0.00002838.eth, false), //    big maker - little taker
          (1.waves,  0.00002838, eth, 1.waves,  0.00002838, eth, -0.00000568.eth, -0.00002838.eth, true),  // symmetric, MARKET taker
          (2.waves,  0.00002838, eth, 10.waves, 0.00002838, eth, -0.00000568.eth, -0.00000567.eth, true),  // little maker - big MARKET taker
          (10.waves, 0.00002838, eth, 2.waves,  0.00002838, eth, -0.00000113.eth, -0.00002838.eth, true)   //    big maker - little MARKET taker
        )
      ) { (mAmt: Long, mFee: Double, mFeeAsset: Asset, tAmt: Long, tFee: Double, tFeeAsset: Asset, mExpectedBalanceChange: Long, tExpectedBalanceChange: Long, isTMarket: Boolean) =>
        // format: on
        val normalizedMakerFee = normalizeAmountAndFee(mFee, assetDecimalsMap(mFeeAsset))
        val normalizedTakerFee = normalizeAmountAndFee(tFee, assetDecimalsMap(tFeeAsset))

        val makerInitialFeeAssetBalance = wavesNode1.api.balance(maker, mFeeAsset)
        val takerInitialFeeAssetBalance = wavesNode1.api.balance(taker, tFeeAsset)

        val makerOrder = mkOrderDP(maker, wavesUsdPair, SELL, mAmt, 3.0, normalizedMakerFee, mFeeAsset)
        val takerOrder = mkOrderDP(taker, wavesUsdPair, BUY, tAmt, 3.0, normalizedTakerFee, tFeeAsset)

        placeAndAwaitAtDex(makerOrder)
        placeAndAwaitAtNode(takerOrder, isMarketOrder = isTMarket)

        dex1.api.cancelAll(maker)
        dex1.api.cancelAll(taker)

        def printAmount(value: Long, asset: Asset): String = s"${denormalizeAmountAndFee(value, assetDecimalsMap(asset))} $asset"

        withClue(
          s"""
             |maker amount                            = ${printAmount(mAmt, Waves)}
             |maker fee                               = $mFee $mFeeAsset
             |maker initial fee asset balance         = ${printAmount(makerInitialFeeAssetBalance, mFeeAsset)}
             |expected maker fee asset balance change = ${printAmount(mExpectedBalanceChange, mFeeAsset)}
             |expected maker fee asset balance        = ${printAmount(makerInitialFeeAssetBalance + mExpectedBalanceChange, mFeeAsset)}
             |
             |taker amount                            = ${printAmount(tAmt, Waves)}
             |taker fee                               = $tFee $tFeeAsset
             |taker initial fee asset balance         = ${printAmount(takerInitialFeeAssetBalance, tFeeAsset)}
             |expected taker fee asset balance change = ${printAmount(tExpectedBalanceChange, tFeeAsset)}
             |expected taker fee asset balance        = ${printAmount(takerInitialFeeAssetBalance + tExpectedBalanceChange, tFeeAsset)}
             |is taker market                         = $isTMarket
             |
             |""".stripMargin
        ) {
          wavesNode1.api.balance(maker, mFeeAsset) shouldBe makerInitialFeeAssetBalance + mExpectedBalanceChange
          wavesNode1.api.balance(taker, tFeeAsset) shouldBe takerInitialFeeAssetBalance + tExpectedBalanceChange
        }
      }
    }
  }

  "DEX should correctly charge different fees when settings changes" in {

    val offsetInitial = dex1.api.currentOffset
    val offset0       = offsetInitial + 1
    val offset1       = offset0 + 1
    val offset2       = offset1 + 1
    val offset3       = offset2 + 1

    dex1.restartWithNewSuiteConfig(
      ConfigFactory.parseString(
        s"""
           |waves.dex {
           |  price-assets = [ "$UsdId", "WAVES" ]
           |  order-fee {
           |    -1: {
           |      mode = dynamic
           |      dynamic {
           |        base-maker-fee = ${0.003.waves}
           |        base-taker-fee = ${0.003.waves}
           |      }
           |    }
           |    $offset0: {
           |      mode = dynamic
           |      dynamic {
           |        base-maker-fee = ${0.003.waves}
           |        base-taker-fee = ${0.003.waves}
           |      }
           |    }
           |    $offset1: {
           |      mode = dynamic
           |      dynamic {
           |        base-maker-fee = ${0.001.waves}
           |        base-taker-fee = ${0.005.waves}
           |      }
           |    }
           |    $offset3: {
           |      mode = dynamic
           |      dynamic {
           |        base-maker-fee = ${0.002.waves}
           |        base-taker-fee = ${0.004.waves}
           |      }
           |    }
           |  }
           |}
       """.stripMargin
      )
    )

    withClue("maker - DynamicSettings(0.003.waves, 0.003.waves), taker - DynamicSettings(0.001.waves, 0.005.waves), fee in Waves") {

      val makerOrder = mkOrderDP(maker, wavesUsdPair, SELL, 10.waves, 3.00, 0.003.waves)
      val takerOrder = mkOrderDP(taker, wavesUsdPair, BUY, 10.waves, 3.00, 0.005.waves)

      dex1.api.currentOffset shouldBe offsetInitial

      placeAndAwaitAtDex(makerOrder)
      dex1.api.currentOffset shouldBe offset0

      val tx = placeAndAwaitAtNode(takerOrder).head
      dex1.api.currentOffset shouldBe offset1

      tx.getSellMatcherFee shouldBe 0.0006.waves
      tx.getBuyMatcherFee shouldBe 0.005.waves

      dex1.api.cancelAll(maker)
      dex1.api.cancelAll(taker)
    }

    withClue("maker - DynamicSettings(0.001.waves, 0.005.waves), taker (market, 25% filled) - DynamicSettings(0.002.waves, 0.004.waves), fee in ETH") {

      dex1.api.currentOffset shouldBe offset1

      val makerOrder = mkOrderDP(maker, wavesUsdPair, SELL, 10.waves, 3.00, 0.00002838.eth, eth) // 0.005.waves = 0.00002838.eth
      val takerOrder = mkOrderDP(taker, wavesUsdPair, BUY, 40.waves, 3.00, 0.00002271.eth, eth)  // 0.004.waves = 0.00002271.eth

      placeAndAwaitAtDex(makerOrder)
      dex1.api.currentOffset shouldBe offset2

      val tx = placeAndAwaitAtNode(takerOrder, isMarketOrder = true).head

      tx.getSellMatcherFee shouldBe 0.00001419.eth
      tx.getBuyMatcherFee shouldBe 0.00000567.eth

      dex1.api.cancelAll(maker)
      dex1.api.cancelAll(taker)
    }
  }
}
