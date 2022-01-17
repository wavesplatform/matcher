package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{FeeNotEnough, UnexpectedFeeAsset}
import com.wavesplatform.dex.settings.AssetType._

class OrderPercentFeeReceivingTestSuite extends OrderFeeBaseTestSuite {

  val version = 3.toByte
  val assetType = Receiving

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""waves.dex {
       |  allowed-order-versions = [1, 2, 3]
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  order-fee.-1 {
       |    mode = percent
       |    percent {
       |      asset-type = receiving
       |      min-fee = $percentFee
       |      min-fee-in-waves = $percentMinFeeInWaves
       |    }
       |  }
       |}""".stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
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

    s"buy order should be rejected if fee Asset not equal Waves when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          minimalFeeWaves,
          usd
        )
      ) should failWith(UnexpectedFeeAsset.code)
    }

    s"sell order should be rejected if fee Asset not equal USD when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
          wavesUsdPair,
          SELL,
          fullyAmountWaves,
          price,
          minimalFeeWaves,
          Waves
        )
      ) should failWith(UnexpectedFeeAsset.code)
    }

    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {

      val accountBuyer = mkAccountWithBalance(minimalFeeWaves -> Waves, fullyAmountUsd -> usd)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves -> Waves, minimalFee -> usd)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(
        mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = minimalFee, version = version, feeAsset = usd)
      )

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, usd) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(usd, 0L) shouldBe 0L
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer = mkAccountWithBalance(minimalFeeWaves -> Waves, fullyAmountUsd -> usd)
      val accountSeller = mkAccountWithBalance(partiallyAmountWaves -> Waves, minimalFee -> usd)

      log.info(
        s"accountBuyer: ${accountBuyer.publicKey.base58} / ${accountBuyer.toAddress}, " +
        s"accountSeller: ${accountSeller.publicKey.base58} / ${accountSeller.toAddress}"
      )

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(
        mkOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, matcherFee = minimalFee, version = version, feeAsset = usd)
      )

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves + (minimalFeeWaves - partiallyFeeWaves)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe fullyAmountUsd - partiallyAmountUsd
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, usd) shouldBe partiallyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 1.5.waves // 3.75 - 9 / 15 * 3.75
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(usd, 0L) shouldBe fullyAmountUsd - partiallyAmountUsd
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.cancelAllOrdersWithSig(accountBuyer)
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer = mkAccountWithBalance(minimalFeeWaves -> Waves, fullyAmountUsd -> usd)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves -> Waves, tooHighFee -> usd)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(
        mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = tooHighFee, version = version, feeAsset = usd)
      )

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, usd) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, usd) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(usd, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(usd, 0L) shouldBe 0L
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
            tooLowFeeWaves,
            version = version
          )
        ) should failWith(
        FeeNotEnough.code,
        "Required 2.1 WAVES as fee for this order, but given 2.09 WAVES"
      )
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
        ) should failWith(
        FeeNotEnough.code,
        s"Required 2.52 ${UsdId.toString} as fee for this order, but given 2.51 ${UsdId.toString}"
      )
    }
  }
}
