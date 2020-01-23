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
       |  order-fee {
       |    mode = dynamic
       |    dynamic {
       |      base-maker-fee = 100000
       |      base-taker-fee = 500000
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

  "DEX with non-default DynamicSettings " - {

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
}
