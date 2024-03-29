package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.Order.PriceConstant
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{FeeNotEnough, UnexpectedFeeAsset}
import com.wavesplatform.dex.settings.AssetType._
import com.wavesplatform.dex.model.{AcceptedOrder, MatcherModel}
import scala.math.BigDecimal.RoundingMode

class OrderPercentFeeSpendingTestSuite extends OrderFeeBaseTestSuite {

  val version = 3.toByte
  val assetType = Spending

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  allowed-order-versions = [1, 2, 3]
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      default {
       |        mode = percent
       |        percent {
       |          asset-type = spending
       |          min-fee = $percentFee
       |          min-fee-in-waves = $percentMinFeeInWaves
       |        }
       |      }
       |
       |      discount {
       |        asset = "${wct.id}"
       |        value = $discount
       |      }
       |    }
       |  }
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx, IssueWctTx)
    dex1.start()
    upsertAssetRate(usd -> usdRate)
  }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {

    s"order should be rejected if fee Asset not in asset pair when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          minimalFeeWaves,
          btc
        )
      ) should failWith(UnexpectedFeeAsset.code)
    }

    s"buy order should be rejected if fee Asset not equal USD when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          minimalFeeWaves,
          Waves
        )
      ) should failWith(UnexpectedFeeAsset.code)
    }

    s"sell order should be rejected if fee Asset not equal Waves when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
          wavesUsdPair,
          SELL,
          fullyAmountWaves,
          price,
          minimalFeeWaves,
          usd
        )
      ) should failWith(UnexpectedFeeAsset.code)
    }

    s"users should pay correct fee when fee asset-type = $assetType and order fully filled " in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId))
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = IssuedAsset(UsdId)
        )
      )
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    // in this suite we're increasing usd rate, decreasing wctRate, and checking that
    // correct values used in order fee calculation
    // matcher should use least wctRate and most usdRate
    s"users should pay correct fee when fee asset-type = $assetType, " +
    s"order fully filled, " +
    s"rate recently changed and " +
    s"fee payed in discount asset" in {
      val correctUsdRate = MatcherModel.correctRateByAssetDecimals(usdRate * 2, assetDecimalsMap(usd))
      val correctWctRate = MatcherModel.correctRateByAssetDecimals(wctRate / 2, assetDecimalsMap(wct))

      dex1.api.upsertAssetRate(wct, wctRate / 2)
      dex1.api.upsertAssetRate(usd, usdRate * 2)

      val feeInUsd = BigDecimal(fullyAmountWaves * price / PriceConstant * percentFee / 100)
        .bigDecimal
        .setScale(0, RoundingMode.CEILING)
        .longValue

      val feeInWct = (BigDecimal(feeInUsd * discount / 100) * correctWctRate / correctUsdRate)
        .setScale(0, RoundingMode.CEILING)
        .toLong

      val accountBuyer = mkAccountWithBalance(fullyAmountUsd -> usd, feeInWct -> wct)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = feeInWct,
          version = version,
          feeAsset = wct
        )
      )
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"taker should pay correct fee when fee asset-type = $assetType and order fully filled and execution price is less than placement price" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd * 2 + 5.04.usd -> IssuedAsset(UsdId))
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + 2.1.waves -> Waves)

      placeAndAwaitAtDex(
        mkOrder(
          accountSeller,
          wavesUsdPair,
          SELL,
          fullyAmountWaves,
          price,
          matcherFee = 2.1.waves,
          version = version
        )
      )

      placeAndAwaitAtNode(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price * 2,
          matcherFee = 5.04.usd,
          feeAsset = IssuedAsset(UsdId),
          version = version
        )
      )

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe (fullyAmountUsd + 2.52.usd)
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId))
      val accountSeller = mkAccountWithBalance(partiallyAmountWaves + partialMinimalFeeWaves -> Waves)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = IssuedAsset(UsdId)
        )
      )
      placeAndAwaitAtNode(mkOrder(
        accountSeller,
        wavesUsdPair,
        SELL,
        partiallyAmountWaves,
        price,
        matcherFee = partialMinimalFeeWaves,
        version = version
      ))

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe fullyAmountUsd + minimalFee - partiallyAmountUsd - partialMinimalFee
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe partiallyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api
        .getReservedBalanceWithApiKey(accountBuyer)
        .getOrElse(IssuedAsset(UsdId), 0L) shouldBe fullyAmountUsd - partiallyAmountUsd + minimalFee - partialMinimalFee
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.cancelAllOrdersWithSig(accountBuyer)
    }

    s"order should be processed and only the required fee should be taken even if a user tries to pay more fee for asset-type = $assetType" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId))
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = IssuedAsset(UsdId)
        )
      )
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = tooHighFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) shouldBe tooHighFeeWaves - minimalFeeWaves
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"const min fee should be taken if order executed fee is less than it for asset-type = $assetType" +
    s" (only for buy orders cuz sell orders do not depend on execution price)" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd -> usd)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves -> Waves)

      //usd rate = 10
      //usd rate corrected = 0.00001
      //buyer price = 10^8
      //min fee = 60 * 10^8 / 10^8 * 0.14 = 8.4 -> 9
      //const min fee = 300000 * 0.00001 = 3

      //exec price = 10^7
      //exec min fee = 60 * 10^7 / 10^8 * 0.14 -> 0.84 -> 1

      placeAndAwaitAtDex(
        mkOrder(accountSeller, wavesUsdPair, SELL, 60L, 1000_0000L, matcherFee = percentMinFeeInWaves, version = version)
      )
      placeAndAwaitAtNode(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          60L,
          10000_0000L,
          matcherFee = 9,
          version = version,
          feeAsset = usd
        )
      )

      val pa = AcceptedOrder.calcAmountOfPriceAsset(60L, 1000_0000L)

      wavesNode1.api.balance(accountBuyer, Waves) should be(60L)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe fullyAmountUsd - pa - 3 //exec min fee = 1, but const min fee = 3
      wavesNode1.api.balance(accountSeller, Waves) shouldBe fullyAmountWaves - 60 - percentMinFeeInWaves
      wavesNode1.api.balance(accountSeller, usd) shouldBe pa

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(usd, 0L) shouldBe 0L
    }

    "price correction should be applied only for taker buyer" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd -> usd)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves -> Waves)

      //buyer is maker here and seller is taker
      //buyer fee shouldn't be corrected and he should pay 8
      //if seller fee is corrected the result-fee will be:
      //3 * 10^5 * 10^8/10^7 = 3 * 10^6 which is more than attached fee (3 * 10^5), so tx will fail
      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          60L,
          10000_0000L,
          matcherFee = 9,
          version = version,
          feeAsset = usd
        )
      )
      placeAndAwaitAtNode(
        mkOrder(accountSeller, wavesUsdPair, SELL, 60L, 1000_0000L, matcherFee = percentMinFeeInWaves, version = version)
      )

      val pa = AcceptedOrder.calcAmountOfPriceAsset(60L, 10000_0000L)
      wavesNode1.api.balance(accountBuyer, Waves) should be(60L)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe fullyAmountUsd - pa - 9
      wavesNode1.api.balance(accountSeller, Waves) shouldBe fullyAmountWaves - 60 - percentMinFeeInWaves
      wavesNode1.api.balance(accountSeller, usd) shouldBe pa

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(usd, 0L) shouldBe 0L
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.tryApi
        .place(
          mkOrder(
            mkAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId)),
            wavesUsdPair,
            BUY,
            fullyAmountWaves,
            price,
            tooLowFee,
            version = version,
            feeAsset = IssuedAsset(UsdId)
          )
        ) should failWith(
        FeeNotEnough.code,
        s"Required 2.52 ${UsdId.toString} as fee for this order, but given 2.51 ${UsdId.toString}"
      )
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.tryApi
        .place(
          mkOrder(
            mkAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves),
            wavesUsdPair,
            SELL,
            fullyAmountWaves,
            price,
            tooLowFeeWaves,
            version = version
          )
        ) should failWith(
        FeeNotEnough.code,
        "Required 2.1 WAVES as fee for this order, but given 2.09 WAVES"
      )
    }
  }

  s"buy order should be rejected if fee less then min-fee-in-waves when fee asset-type = $assetType" in {
    val correctRate = MatcherModel.correctRateByAssetDecimals(usdRate, usdAssetDecimals)

    val minFeeInUsd = (BigDecimal(percentMinFeeInWaves) * correctRate)
      .setScale(0, RoundingMode.CEILING)
      .toLong

    dex1.tryApi
      .place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId)),
          wavesUsdPair,
          BUY,
          percentMinFeeInWaves * 10,
          price,
          minFeeInUsd - 1,
          version = version,
          feeAsset = IssuedAsset(UsdId)
        )
      ) should failWith(
      FeeNotEnough.code,
      s"Required 0.03 ${UsdId.toString} as fee for this order, but given 0.02 ${UsdId.toString}"
    )
  }

  s"sell order should be rejected if fee less then min-fee-in-waves when fee asset-type = $assetType" in {
    dex1.tryApi
      .place(
        mkOrder(
          mkAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves),
          wavesUsdPair,
          SELL,
          percentMinFeeInWaves * 5,
          price,
          percentMinFeeInWaves - 1,
          version = version
        )
      ) should failWith(
      FeeNotEnough.code,
      "Required 0.003 WAVES as fee for this order, but given 0.00299999 WAVES"
    )
  }
}
