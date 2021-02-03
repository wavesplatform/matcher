package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.settings.AssetType._

class OrderPercentFeePriceTestSuite extends OrderFeeBaseTestSuite {
  val version = 3.toByte
  val assetType = Price

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  allowed-order-versions = [1, 2, 3]
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = percent
       |    percent {
       |      asset-type = price
       |      min-fee = $percentFee
       |    }
       |  }
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {
    s"users should pay correct fee when fee asset-type = $assetType and order fully filled " in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd + minimalFee -> usd)
      val accountSeller = mkAccountWithBalance(minimalFee -> usd, fullyAmountWaves -> Waves)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = usd
        )
      )
      placeAndAwaitAtNode(
        mkOrder(
          accountSeller,
          wavesUsdPair,
          SELL,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = usd
        )
      )

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, usd) shouldBe fullyAmountUsd

      dex1.api.getReservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountBuyer).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountSeller).getOrElse(usd, 0L) shouldBe 0L
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd + minimalFee -> usd)
      val accountSeller = mkAccountWithBalance(partiallyAmountWaves -> Waves, minimalFee -> usd)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = usd
        )
      )
      placeAndAwaitAtNode(
        mkOrder(
          accountSeller,
          wavesUsdPair,
          SELL,
          partiallyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = usd
        )
      )

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves
      wavesNode1.api.balance(accountBuyer, usd) shouldBe partiallyAmountUsd - (minimalFee - partiallyFeeUsd)
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, usd) shouldBe partiallyAmountUsd

      dex1.api.getReservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api
        .getReservedBalance(accountBuyer)
        .getOrElse(usd, 0L) shouldBe fullyAmountUsd - partiallyAmountUsd + (minimalFee - partiallyFeeUsd)
      dex1.api.getReservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountSeller).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.cancelAll(accountBuyer)
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd + minimalFee -> usd)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves -> Waves, tooHighFee -> usd)

      placeAndAwaitAtDex(
        mkOrder(
          accountBuyer,
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          matcherFee = minimalFee,
          version = version,
          feeAsset = usd
        )
      )
      placeAndAwaitAtNode(
        mkOrder(
          accountSeller,
          wavesUsdPair,
          SELL,
          fullyAmountWaves,
          price,
          matcherFee = tooHighFee,
          version = version,
          feeAsset = usd
        )
      )

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, usd) shouldBe fullyAmountUsd

      dex1.api.getReservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountBuyer).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalance(accountSeller).getOrElse(usd, 0L) shouldBe 0L
    }

    s"buy order should be rejected user will get tokens for pay fee after order executed when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd -> usd),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          minimalFee,
          version = version,
          feeAsset = usd
        )
      ) should failWithBalanceNotEnough(required = Map(usd -> 22.5.usd), available = Map(usd -> 18.usd))
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.tryApi
        .place(
          mkOrder(
            mkAccountWithBalance(fullyAmountUsd + minimalFee -> usd),
            wavesUsdPair,
            BUY,
            fullyAmountWaves,
            price,
            tooLowFee,
            version = version,
            feeAsset = usd
          )
        ) should failWith(9441542, s"Required 2.52 ${UsdId.toString} as fee for this order, but given 2.51 ${UsdId.toString}")
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.tryApi
        .place(
          mkOrder(
            mkAccountWithBalance(fullyAmountWaves -> Waves, minimalFee -> usd),
            wavesUsdPair,
            SELL,
            fullyAmountWaves,
            price,
            tooLowFee,
            version = version,
            feeAsset = usd
          )
        ) should failWith(9441542, s"Required 2.52 ${UsdId.toString} as fee for this order, but given 2.51 ${UsdId.toString}")
    }
  }
}
