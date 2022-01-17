package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.error.{FeeNotEnough, UnexpectedFeeAsset}
import com.wavesplatform.dex.settings.AssetType._

class V1OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(1.toByte)
class V2OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(2.toByte)

class V3OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(3.toByte) {

  s"order should be rejected if fee Asset not in asset pair when fee asset-type = $assetType" in {
    dex1.tryApi.place(
      mkOrder(
        mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
        wavesUsdPair,
        BUY,
        fullyAmountWaves,
        price,
        minimalFeeWaves,
        usdn
      )
    ) should failWith(UnexpectedFeeAsset.code)
  }

  s"buy order should be rejected if fee Asset not equal WAVES when fee asset-type = $assetType" in {
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

  s"sell order should be rejected if fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.tryApi.place(
      mkOrder(
        mkAccountWithBalance(minimalFeeWaves -> usd, fullyAmountWaves -> Waves),
        wavesUsdPair,
        SELL,
        fullyAmountWaves,
        price,
        minimalFeeWaves,
        usd
      )
    ) should failWith(UnexpectedFeeAsset.code)
  }
}

abstract class OrderPercentFeeAmountTestSuite(version: Byte) extends OrderFeeBaseTestSuite {
  protected val assetType = Amount

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee.-1 {
         |    mode = percent
         |    percent {
         |      asset-type = amount
         |      min-fee = $percentFee
         |      min-fee-in-waves = $percentMinFeeInWaves
         |    }
         |  }
         |  price-assets = [ "$UsdId" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueUsdnTx)
    dex1.start()
    upsertAssetRate(usd -> usdRate)
  }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {

    s"users should pay correct fee when fee asset-type = $assetType and order fully filled " in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd -> IssuedAsset(UsdId), minimalFeeWaves -> Waves)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
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

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd -> IssuedAsset(UsdId), minimalFeeWaves -> Waves)
      val accountSeller = mkAccountWithBalance(partiallyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves - partiallyFeeWaves + minimalFeeWaves
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe fullyAmountUsd - partiallyAmountUsd
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe partiallyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe minimalFeeWaves - partiallyFeeWaves
      dex1.api
        .getReservedBalanceWithApiKey(accountBuyer)
        .getOrElse(IssuedAsset(UsdId), 0L) shouldBe fullyAmountUsd - partiallyAmountUsd
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.cancelAllOrdersWithSig(accountBuyer)
    }

    s"order should be processed if amount less than fee when fee asset-type = $assetType" in {
      val accountBuyer = mkAccountWithBalance(fullyAmountUsd -> IssuedAsset(UsdId), minimalFeeWaves -> Waves)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = tooHighFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.getReservedBalanceWithApiKey(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"buy order should be rejected user will get tokens for pay fee after order executed when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          createAccountWithBalance(fullyAmountUsd -> usd),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          minimalFeeWaves,
          version = version
        )
      ) should failWithBalanceNotEnough(required = Map(Waves -> 3.75.waves, usd -> 18.usd), available = Map(Waves -> 0.waves, usd -> 18.usd))
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountUsd -> usd, minimalFeeWaves -> Waves),
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

    s"sell order should be rejected if fee less than minimum possible fee when fee asset-type = $assetType" in {
      dex1.tryApi.place(
        mkOrder(
          mkAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves),
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
}
