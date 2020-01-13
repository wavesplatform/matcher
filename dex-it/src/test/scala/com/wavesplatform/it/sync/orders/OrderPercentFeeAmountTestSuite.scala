package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dex.settings.AssetType._
import com.wavesplatform.dex.settings.FeeMode._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
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
         |      min-fee = 25
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

  private def balancesShouldBe(account: KeyPair, balances: (Asset, Long)*): Unit =
    balances.foreach { case (asset, balance) => wavesNode1.api.balance(account, asset) should be(balance) }

  private def reservedBalancesShouldBe(account: KeyPair, balances: (Asset, Long)*): Unit =
    balances.foreach { case (asset, balance) => dex1.api.reservedBalance(account).getOrElse(asset, 0L) shouldBe balance }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {
    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                     -> usd)
      val accountSeller = createAccountWithBalance(fullyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, minimalFeeWaves, version = version))

      balancesShouldBe(accountBuyer, usd  -> 0L, Waves             -> (fullyAmountWaves - minimalFeeWaves))
      balancesShouldBe(accountSeller, usd -> fullyAmountUsd, Waves -> 0L)

      reservedBalancesShouldBe(accountBuyer, usd  -> 0L, Waves -> 0L)
      reservedBalancesShouldBe(accountSeller, usd -> 0L, Waves -> 0L)
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                         -> usd)
      val accountSeller = createAccountWithBalance(partiallyAmountWaves + minimalFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, minimalFeeWaves, version = version))

      balancesShouldBe(accountBuyer, usd  -> partiallyAmountUsd, Waves -> (partiallyAmountWaves - partiallyFeeWaves))
      balancesShouldBe(accountSeller, usd -> partiallyAmountUsd, Waves -> 0L)

      reservedBalancesShouldBe(accountBuyer, usd  -> partiallyAmountUsd, Waves -> 0L)
      reservedBalancesShouldBe(accountSeller, usd -> 0L, Waves                 -> 0L)

      List(accountBuyer, accountSeller).foreach(dex1.api.cancelAll(_))
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd                     -> usd)
      val accountSeller = createAccountWithBalance(fullyAmountWaves + tooHighFeeWaves -> Waves)

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFeeWaves, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, tooHighFeeWaves, version = version))

      balancesShouldBe(accountBuyer, usd  -> 0L, Waves             -> (fullyAmountWaves - minimalFeeWaves))
      balancesShouldBe(accountSeller, usd -> fullyAmountUsd, Waves -> 1.waves)
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
        "Required 37.5 WAVES as fee for this order, but given 37.49999 WAVES"
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
        "Required 37.5 WAVES as fee for this order, but given 37.49999 WAVES"
      )
    }
  }
}
