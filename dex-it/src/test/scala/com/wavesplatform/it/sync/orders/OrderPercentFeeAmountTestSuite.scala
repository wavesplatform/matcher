package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.{IssuedAsset, Waves}
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.settings.AssetType._
import com.wavesplatform.dex.settings.FeeMode._

class V1OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(1.toByte)
class V2OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(2.toByte)
class V3OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(3.toByte) {
  s"buy order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.api.tryPlace(
      mkOrder(
        mkAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
        wavesUsdPair,
        BUY,
        fullyAmountWaves,
        price,
        minimalFeeWaves,
        usd
      )
    ) should failWith(9441540) // UnexpectedFeeAsset
  }

  s"sell order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.api.tryPlace(
      mkOrder(
        mkAccountWithBalance(minimalFeeWaves -> usd, fullyAmountWaves -> Waves),
        wavesUsdPair,
        SELL,
        fullyAmountWaves,
        price,
        minimalFeeWaves,
        usd
      )
    ) should failWith(9441540) // UnexpectedFeeAsset
  }
}

abstract class OrderPercentFeeAmountTestSuite(version: Byte) extends OrderFeeBaseTestSuite {
  protected val assetType = Amount

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee.-1 {
         |    mode = $Percent
         |    $Percent {
         |      asset-type = $assetType
         |      min-fee = $percentFee
         |    }
         |  }
         |  price-assets = [ "$UsdId" ]
         |}""".stripMargin
    )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx)
    dex1.start()
  }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {

    s"users should pay correct fee when fee asset-type = $assetType and order fully filled " in {
      val accountBuyer  = mkAccountWithBalance(fullyAmountUsd                     -> IssuedAsset(UsdId), minimalFeeWaves -> Waves)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer  = mkAccountWithBalance(fullyAmountUsd                         -> IssuedAsset(UsdId), minimalFeeWaves -> Waves)
      val accountSeller = mkAccountWithBalance(partiallyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves - partiallyFeeWaves + minimalFeeWaves
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe fullyAmountUsd - partiallyAmountUsd
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe partiallyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe minimalFeeWaves - partiallyFeeWaves
      dex1.api
        .reservedBalance(accountBuyer)
        .getOrElse(IssuedAsset(UsdId), 0L) shouldBe fullyAmountUsd - partiallyAmountUsd
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.cancelAll(accountBuyer)
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer  = mkAccountWithBalance(fullyAmountUsd                     -> IssuedAsset(UsdId), minimalFeeWaves -> Waves)
      val accountSeller = mkAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = tooHighFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"buy order should be rejected user will get tokens for pay fee after order executed when fee asset-type = $assetType" in {
      dex1.api.tryPlace(
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
      dex1.api.tryPlace(
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
        9441542, // FeeNotEnough
        "Required 2.1 WAVES as fee for this order, but given 2.09 WAVES"
      )
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api.tryPlace(
        mkOrder(mkAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves),
                wavesUsdPair,
                SELL,
                fullyAmountWaves,
                price,
                tooLowFeeWaves,
                version = version)
      ) should failWith(
        9441542, // FeeNotEnough
        "Required 2.1 WAVES as fee for this order, but given 2.09 WAVES"
      )
    }
  }
}
