package com.wavesplatform.it.matcher.api.http.markets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order.PriceConstant
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.it.api.RawHttpChecks
import com.wavesplatform.dex.model.MatcherModel
import com.wavesplatform.it.MatcherSuiteBase

import scala.math.BigDecimal.RoundingMode

final class CalculateFeeSpec extends MatcherSuiteBase with RawHttpChecks {

  private val btcRate = 0.001
  private val usdRate = 0.02
  private val discountFactor = 0.4
  private val percentFeeFactor = 0.03
  private val assetPair = AssetPair(Waves, usd)

  "CalculateFeeSpec" - {

    "POST /matcher/orderbook/{amountAsset}/{priceAsset}/calculateFee should calculate fee based on dynamic fee settings config" in {
      List(btc -> btcRate, usd -> usdRate).foreach { case (asset, rate) => dex1.api.upsertAssetRate(asset, rate) }
      val calculatedFee = dex1.api.calculateFee(assetPair, OrderType.BUY, 100.waves, 1.usd)
      calculatedFee.base.value.feeAssetId shouldBe Waves
      calculatedFee.base.value.matcherFee shouldBe matcherFee
      calculatedFee.discount.value.feeAssetId shouldBe btc
      calculatedFee.discount.value.matcherFee shouldBe (matcherFee * btcRate * (1 - discountFactor)).longValue()
    }

    "POST /matcher/orderbook/{amountAsset}/{priceAsset}/calculateFee should calculate fee based on percent fee settings config" in {
      List(btc -> btcRate, usd -> usdRate).foreach { case (asset, rate) => dex1.api.upsertAssetRate(asset, rate) }
      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          """ waves.dex.order-fee.-1.composite.default.mode = percent """.stripMargin
        ).withFallback(dexInitialSuiteConfig)
      )

      val calculatedFee = dex1.api.calculateFee(assetPair, OrderType.BUY, 100.waves, 1.usd)
      calculatedFee.base.value.feeAssetId shouldBe Waves
      calculatedFee.base.value.matcherFee shouldBe (100.waves * percentFeeFactor).longValue()
      calculatedFee.discount.value.feeAssetId shouldBe btc
      calculatedFee.discount.value.matcherFee shouldBe (100.waves * percentFeeFactor * btcRate * (1 - discountFactor)).longValue()
    }

    "POST /matcher/orderbook/{amountAsset}/{priceAsset}/calculateFee should calculate fee based on " +
    "percent fee settings " +
    "with asset type 'spending' " +
    "with correct rates (least btcRate and most usdRate)" in {
      val correctedBtcRate = MatcherModel.correctRateByAssetDecimals(btcRate / 2, assetDecimalsMap(btc))
      val correctedUsdRate = MatcherModel.correctRateByAssetDecimals(usdRate * 2, assetDecimalsMap(usd))

      dex1.api.upsertAssetRate(btc, btcRate)
      dex1.api.upsertAssetRate(btc, btcRate / 2)

      dex1.api.upsertAssetRate(usd, usdRate)
      dex1.api.upsertAssetRate(usd, usdRate * 2)

      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          s"""
             |waves.dex.order-fee.-1.composite.default {
             |  mode = percent
             |  percent {
             |    asset-type = spending
             |    min-fee = ${(percentFeeFactor * 100).longValue()}
             |    min-fee-in-waves = $matcherFee
             |  }
             |}
          """.stripMargin
        ).withFallback(dexInitialSuiteConfig)
      )

      val price = 1.usd
      val amount = 100.eth

      val calculatedFee = dex1.api.calculateFee(AssetPair(eth, usd), OrderType.BUY, amount, price)

      val expectedBaseFee = BigDecimal(amount * price) / PriceConstant * percentFeeFactor
      val expectedDiscountFee = expectedBaseFee * correctedBtcRate / correctedUsdRate * (1 - discountFactor)

      calculatedFee.base.value.feeAssetId shouldBe usd
      calculatedFee.base.value.matcherFee shouldBe expectedBaseFee.setScale(0, RoundingMode.CEILING).longValue
      calculatedFee.discount.value.feeAssetId shouldBe btc
      calculatedFee.discount.value.matcherFee shouldBe expectedDiscountFee.setScale(0, RoundingMode.CEILING).longValue
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueBtcTx, IssueUsdTx, IssueEthTx)
    dex1.start()
  }

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""
         |waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee.-1 {
         |    mode = composite
         |    composite {
         |      default {
         |        mode = dynamic
         |        dynamic {
         |          base-maker-fee = $matcherFee
         |          base-taker-fee = $matcherFee
         |        }
         |        percent {
         |          asset-type = amount
         |          min-fee = ${(percentFeeFactor * 100).longValue()}
         |          min-fee-in-waves = $matcherFee
         |        }
         |      }
         |      discount {
         |        asset = "$BtcId"
         |        value = ${(discountFactor * 100).longValue()}
         |      }
         |    }
         |  }
         |}
       """.stripMargin
    )

}
