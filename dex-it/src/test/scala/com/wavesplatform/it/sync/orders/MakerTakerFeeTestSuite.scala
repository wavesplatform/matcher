package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.model.Denormalization._
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.it.MatcherSuiteBase

class MakerTakerFeeTestSuite extends MatcherSuiteBase {

  private val maker = bob
  private val taker = alice

  override protected val dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "WAVES" ]
       |  order-fee {
       |    mode = dynamic
       |    dynamic {
       |      base-fee = 300000
       |      zero-maker-double-taker = true
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueEthTx)
    dex1.start()
    dex1.api.upsertRate(eth, 0.00567593) // 0.003.waves = 0.00001703.eth
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    dex1.api.cancelAll(maker)
    dex1.api.cancelAll(taker)
  }

  private def matchMakerWithTaker(makerAmount: Long,
                                  makerFee: Long = 0.006.waves,
                                  makerFeeAsset: Asset = Waves,
                                  takerAmount: Long,
                                  takerFee: Long = 0.006.waves,
                                  takerFeeAsset: Asset = Waves,
                                  expectedMakerFeeAssetBalanceChange: Long,
                                  expectedTakerFeeAssetBalanceChange: Long,
                                  isTakerMarket: Boolean = false): Unit = {

    val makerInitialFeeAssetBalance = wavesNode1.api.balance(maker, makerFeeAsset)
    val takerInitialFeeAssetBalance = wavesNode1.api.balance(taker, takerFeeAsset)

    val makerOrder = mkOrderDP(maker, wavesUsdPair, SELL, makerAmount, 3.0, makerFee, makerFeeAsset)
    val takerOrder = mkOrderDP(taker, wavesUsdPair, BUY, takerAmount, 3.0, takerFee, takerFeeAsset)

    placeAndAwaitAtDex(makerOrder)
    placeAndAwaitAtNode(takerOrder, isMarketOrder = isTakerMarket)

    def printAmount(value: Long, asset: Asset): String = s"${denormalizeAmountAndFee(value, assetDecimalsMap(asset))} $asset"

    withClue(
      s"""
         |maker amount                            = ${printAmount(makerAmount, Waves)}
         |maker fee                               = ${printAmount(makerFee, makerFeeAsset)}
         |maker initial fee asset balance         = ${printAmount(makerInitialFeeAssetBalance, makerFeeAsset)}
         |expected maker fee asset balance change = ${printAmount(expectedMakerFeeAssetBalanceChange, makerFeeAsset)}
         |expected maker fee asset balance        = ${printAmount(makerInitialFeeAssetBalance + expectedMakerFeeAssetBalanceChange, makerFeeAsset)}
         |
         |taker amount                            = ${printAmount(takerAmount, Waves)}
         |taker fee                               = ${printAmount(takerFee, takerFeeAsset)}
         |taker initial fee asset balance         = ${printAmount(takerInitialFeeAssetBalance, takerFeeAsset)}
         |expected taker fee asset balance change = ${printAmount(expectedTakerFeeAssetBalanceChange, takerFeeAsset)}
         |expected taker fee asset balance        = ${printAmount(takerInitialFeeAssetBalance + expectedTakerFeeAssetBalanceChange, takerFeeAsset)}
         |is taker market                         = $isTakerMarket
         |
         |""".stripMargin
    ) {
      wavesNode1.api.balance(maker, makerFeeAsset) shouldBe makerInitialFeeAssetBalance + expectedMakerFeeAssetBalanceChange
      wavesNode1.api.balance(taker, takerFeeAsset) shouldBe takerInitialFeeAssetBalance + expectedTakerFeeAssetBalanceChange
    }
  }

  "DEX should charge 0 fee for makers and doubled fee for takers" - {

    "rejecting orders with insufficient fee" in {
      dex1.api.tryPlace(mkOrderDP(maker, wavesUsdPair, SELL, 1.waves, 3.00, 0.00599999.waves)) should failWith(
        9441542, // FeeNotEnough
        s"Required 0.006 WAVES as fee for this order, but given 0.00599999 WAVES"
      )
    }

    "symmetric orders" in {
      matchMakerWithTaker(
        makerAmount = 1.waves,
        takerAmount = 1.waves,
        expectedMakerFeeAssetBalanceChange = -1.waves,
        expectedTakerFeeAssetBalanceChange = 1.waves - 0.006.waves
      )
    }

    "symmetric orders with MARKET taker" in {
      matchMakerWithTaker(
        makerAmount = 1.waves,
        takerAmount = 1.waves,
        expectedMakerFeeAssetBalanceChange = -1.waves,
        expectedTakerFeeAssetBalanceChange = 1.waves - 0.006.waves,
        isTakerMarket = true
      )
    }

    "little maker and big taker" in {
      matchMakerWithTaker(
        makerAmount = 1.waves,
        takerAmount = 10.waves,
        expectedMakerFeeAssetBalanceChange = -1.waves,
        expectedTakerFeeAssetBalanceChange = 1.waves - 0.0006.waves
      )
    }

    "little maker and big MARKET taker" in {
      matchMakerWithTaker(
        makerAmount = 1.waves,
        takerAmount = 10.waves,
        expectedMakerFeeAssetBalanceChange = -1.waves,
        expectedTakerFeeAssetBalanceChange = 1.waves - 0.0006.waves,
        isTakerMarket = true
      )
    }

    "big maker and little taker" in {
      matchMakerWithTaker(
        makerAmount = 10.waves,
        takerAmount = 1.waves,
        expectedMakerFeeAssetBalanceChange = -1.waves,
        expectedTakerFeeAssetBalanceChange = 1.waves - 0.006.waves
      )
    }

    "big maker and little MARKET taker" in {
      matchMakerWithTaker(
        makerAmount = 10.waves,
        takerAmount = 1.waves,
        expectedMakerFeeAssetBalanceChange = -1.waves,
        expectedTakerFeeAssetBalanceChange = 1.waves - 0.006.waves,
        isTakerMarket = true
      )
    }
  }
}
