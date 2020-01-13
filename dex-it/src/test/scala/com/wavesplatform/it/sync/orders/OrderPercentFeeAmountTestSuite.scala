package com.wavesplatform.it.sync.orders

import java.nio.charset.StandardCharsets
import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.settings.AssetType._
import com.wavesplatform.dex.settings.FeeMode._
import com.wavesplatform.it.MatcherSuiteBase

class V1OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(1.toByte)
class V2OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(2.toByte)
class V3OrderPercentFeeAmountTestSuite extends OrderPercentFeeAmountTestSuite(3.toByte) {
  s"buy order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.api.tryPlace(
      mkOrder(
        createAccountWithBalance(usd -> (fullyAmountUsd + minimalFee), Waves -> minimalFee),
        wavesUsdPair,
        BUY,
        fullyAmountWaves,
        price,
        minimalFee,
        usd
      )) should failWith(9441540) // UnexpectedFeeAsset
  }

  s"sell order should be rejected is fee Asset not equal WAVES when fee asset-type = $assetType" in {
    dex1.api.tryPlace(
      mkOrder(
        createAccountWithBalance(usd -> minimalFee, Waves -> fullyAmountWaves),
        wavesUsdPair,
        SELL,
        fullyAmountWaves,
        price,
        minimalFee,
        usd
      )) should failWith(9441540) // UnexpectedFeeAsset
  }
}

abstract class OrderPercentFeeAmountTestSuite(version: Byte) extends MatcherSuiteBase {
  protected val assetType = AMOUNT

  protected val price            = 0.4.usd
  protected val fullyAmountWaves = 150.waves
  protected val fullyAmountUsd   = 60.usd
  protected val minimalFee       = 37.5.waves

  private val tooLowFee            = 37.49999.waves
  private val tooHighFee           = 150.00001.waves
  private val partiallyAmountWaves = 75.waves
  private val partiallyAmountUsd   = 30.usd
  private val partiallyFeeWaves    = 18.75.waves

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

  protected def createAccountWithBalance(balances: (Asset, Long)*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${ThreadLocalRandom.current().nextInt()}".getBytes(StandardCharsets.UTF_8)))
    balances.foreach { case (asset, balance) => broadcastAndAwait(mkTransfer(alice, account, balance, asset)) }
    account
  }

  private def balancesShouldBe(account: KeyPair, balances: (Asset, Long)*): Unit =
    balances.foreach { case (asset, balance) => wavesNode1.api.balance(account, asset) should be(balance) }

  private def reservedBalancesShouldBe(account: KeyPair, balances: (Asset, Long)*): Unit =
    balances.foreach { case (asset, balance) => dex1.api.reservedBalance(account).getOrElse(asset, 0L) shouldBe balance }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {
    s"users should pay correct fee when fee asset-type = $assetType and order fully filled" in {
      val accountBuyer  = createAccountWithBalance(usd   -> fullyAmountUsd)
      val accountSeller = createAccountWithBalance(Waves -> (fullyAmountWaves + minimalFee))

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, minimalFee, version = version))

      balancesShouldBe(accountBuyer, usd  -> 0L, Waves             -> (fullyAmountWaves - minimalFee))
      balancesShouldBe(accountSeller, usd -> fullyAmountUsd, Waves -> 0L)

      reservedBalancesShouldBe(accountBuyer, usd  -> 0L, Waves -> 0L)
      reservedBalancesShouldBe(accountSeller, usd -> 0L, Waves -> 0L)
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer  = createAccountWithBalance(usd   -> fullyAmountUsd)
      val accountSeller = createAccountWithBalance(Waves -> (partiallyAmountWaves + minimalFee))

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, partiallyAmountWaves, price, minimalFee, version = version))

      balancesShouldBe(accountBuyer, usd  -> partiallyAmountUsd, Waves -> (partiallyAmountWaves - partiallyFeeWaves))
      balancesShouldBe(accountSeller, usd -> partiallyAmountUsd, Waves -> 0L)

      reservedBalancesShouldBe(accountBuyer, usd  -> partiallyAmountUsd, Waves -> 0L)
      reservedBalancesShouldBe(accountSeller, usd -> 0L, Waves                 -> 0L)

      List(accountBuyer, accountSeller).foreach(dex1.api.cancelAll(_))
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer  = createAccountWithBalance(usd   -> fullyAmountUsd)
      val accountSeller = createAccountWithBalance(Waves -> (1.waves + fullyAmountWaves + tooHighFee))

      placeAndAwaitAtDex(mkOrder(accountBuyer, wavesUsdPair, BUY, fullyAmountWaves, price, minimalFee, version = version))
      placeAndAwaitAtNode(mkOrder(accountSeller, wavesUsdPair, SELL, fullyAmountWaves, price, tooHighFee, version = version))

      balancesShouldBe(accountBuyer, usd  -> 0L, Waves             -> (fullyAmountWaves - minimalFee))
      balancesShouldBe(accountSeller, usd -> fullyAmountUsd, Waves -> 1.waves)
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api.tryPlace(
        mkOrder(
          createAccountWithBalance(usd -> fullyAmountUsd, Waves -> minimalFee),
          wavesUsdPair,
          BUY,
          fullyAmountWaves,
          price,
          tooLowFee,
          version = version
        )) should failWith(
        9441542, // FeeNotEnough
        "Required 37.5 WAVES as fee for this order, but given 37.49999 WAVES"
      )
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api.tryPlace(
        mkOrder(createAccountWithBalance(Waves -> (fullyAmountWaves + minimalFee)),
                wavesUsdPair,
                SELL,
                fullyAmountWaves,
                price,
                tooLowFee,
                version = version)) should failWith(
        9441542, // FeeNotEnough
        "Required 37.5 WAVES as fee for this order, but given 37.49999 WAVES"
      )
    }
  }
}
