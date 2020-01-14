package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.settings.AssetType._
import com.wavesplatform.dex.settings.FeeMode._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class V1OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(1.toByte)
class V2OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(2.toByte)
class V3OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(3.toByte) {
  s"buy order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.api.tryPlace(
      mkOrder(
        createAccountWithBalance(fullyAmountUsd + minimalFeeWaves -> usd, minimalFeeWaves -> Waves),
        wavesUsdPair,
        BUY,
        fullyAmountWaves,
        price,
        minimalFeeWaves,
        usd
      )) should failWith(9441540) // UnexpectedFeeAsset
  }

  s"sell order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.api.tryPlace(
      mkOrder(
        createAccountWithBalance(minimalFeeWaves -> usd, fullyAmountWaves -> Waves),
        wavesUsdPair,
        SELL,
        fullyAmountWaves,
        price,
        minimalFeeWaves,
        usd
      )) should failWith(9441540) // UnexpectedFeeAsset
  }
}

abstract class OrderPercentFeeAmountTestSuite(version: Byte) extends OrderFeeBaseTestSuite {
  protected val assetType = AMOUNT

  override protected val dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = $PERCENT
         |    $PERCENT {
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
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                     -> IssuedAsset(UsdId))
      val accountSeller = createAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves - minimalFeeWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                         -> IssuedAsset(UsdId))
      val accountSeller = createAccountWithBalance(partiallyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves - partiallyFeeWaves
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe fullyAmountUsd - partiallyAmountUsd
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe partiallyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api
        .reservedBalance(accountBuyer)
        .getOrElse(IssuedAsset(UsdId), 0L) shouldBe fullyAmountUsd - partiallyAmountUsd
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.cancelAll(accountBuyer)
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                     -> IssuedAsset(UsdId))
      val accountSeller = createAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, matcherFee = minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, matcherFee = tooHighFeeWaves, version = version))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves - minimalFeeWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api.tryPlace(
        mkOrder(
          createAccountWithBalance(fullyAmountUsd -> usd, minimalFeeWaves -> Waves),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          tooLowFeeWaves,
          version = version
        )) should failWith(
        9441542, // FeeNotEnough
        "Required 2.1 WAVES as fee for this order, but given 2.09 WAVES"
      )
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api.tryPlace(
        mkOrder(createAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves),
                wavesUsdPair,
                SELL,
                fullyAmountWaves,
                price,
                tooLowFeeWaves,
                version = version)) should failWith(
        9441542, // FeeNotEnough
        "Required 2.1 WAVES as fee for this order, but given 2.09 WAVES"
      )
    }
  }
}
