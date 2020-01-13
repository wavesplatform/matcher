package com.wavesplatform.it.sync.orders

import java.nio.charset.StandardCharsets

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.dex.settings.FeeMode.PERCENT
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class OrderPercentFeePriceTestSuite extends MatcherSuiteBase {

  val percentFee           = 14
  val version              = 3.toByte
  val assetType            = "price"
  val price                = 1.2.usd
  val fullyAmountWaves     = 15.waves
  val partiallyAmountWaves = 9.waves
  val fullyAmountUsd       = 18.usd
  val minimalFee           = 4.5.usd
  val partiallyFeeUsd      = 2.7.usd
  val partiallyAmountUsd   = 10.8.usd
  val tooLowFee            = 2.51 usd
  val tooHighFee           = 18.01.usd

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(s"""
                                                                                      |waves.dex {
                                                                                      |  allowed-order-versions = [1, 2, 3]
                                                                                      |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
                                                                                      |  order-fee {
                                                                                      |    mode = $PERCENT
                                                                                      |    $PERCENT {
                                                                                      |      asset-type = $assetType
                                                                                      |      min-fee = $percentFee
                                                                                      |    }
                                                                                      |  }
                                                                                      |}""".stripMargin)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    dex1.start()
  }

  def createAccountWithBalance(balances: (Long, Asset)*): KeyPair = {
    val account = KeyPair(ByteStr(s"account-test-${System.currentTimeMillis}".getBytes(StandardCharsets.UTF_8)))

    balances.foreach {
      case (balance, asset) => {
        if (asset != None)
          assert(
            wavesNode1.api.balance(alice, asset) >= balance,
            s"Bob doesn't have enough balance in ${asset.toString} to make a transfer"
          )

        broadcastAndAwait(mkTransfer(alice, account.toAddress, balance, asset))
      }
    }
    account
  }

  s"V$version orders (fee asset type: $assetType) & fees processing" - {
    s"users should pay correct fee when fee asset-type = $assetType and order fully filled " in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId))
      val accountSeller = createAccountWithBalance(fullyAmountWaves            -> Waves)

      placeAndAwaitAtDex(
        mkOrder(accountBuyer,
                wavesUsdPair,
                BUY,
                fullyAmountWaves,
                price,
                matcherFee = minimalFee,
                version = version,
                matcherFeeAssetId = IssuedAsset(UsdId)))
      placeAndAwaitAtNode(
        mkOrder(accountSeller,
                wavesUsdPair,
                SELL,
                fullyAmountWaves,
                price,
                matcherFee = minimalFee,
                version = version,
                matcherFeeAssetId = IssuedAsset(UsdId)))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd - minimalFee

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"users should pay correct fee when fee asset-type = $assetType and order partially filled" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId))
      val accountSeller = createAccountWithBalance(partiallyAmountWaves        -> Waves, minimalFee -> IssuedAsset(UsdId))

      placeAndAwaitAtDex(
        mkOrder(accountBuyer,
                wavesUsdPair,
                BUY,
                fullyAmountWaves,
                price,
                matcherFee = minimalFee,
                version = version,
                matcherFeeAssetId = IssuedAsset(UsdId)))
      placeAndAwaitAtNode(
        mkOrder(accountSeller,
                wavesUsdPair,
                SELL,
                partiallyAmountWaves,
                price,
                matcherFee = minimalFee,
                version = version,
                matcherFeeAssetId = IssuedAsset(UsdId)))

      wavesNode1.api.balance(accountBuyer, Waves) shouldBe partiallyAmountWaves
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe partiallyAmountUsd - (minimalFee - partiallyFeeUsd)
      wavesNode1.api.balance(accountSeller, Waves) shouldBe 0L
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe partiallyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api
        .reservedBalance(accountBuyer)
        .getOrElse(IssuedAsset(UsdId), 0L) shouldBe fullyAmountUsd - partiallyAmountUsd + (minimalFee - partiallyFeeUsd)
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.cancelAll(accountBuyer)
    }

    s"order should be processed if amount less then fee when fee asset-type = $assetType" in {
      val accountBuyer  = createAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId))
      val accountSeller = createAccountWithBalance(fullyAmountWaves            -> Waves, tooHighFee -> IssuedAsset(UsdId))

      placeAndAwaitAtDex(
        mkOrder(accountBuyer,
                wavesUsdPair,
                BUY,
                fullyAmountWaves,
                price,
                matcherFee = minimalFee,
                version = version,
                matcherFeeAssetId = IssuedAsset(UsdId)))
      placeAndAwaitAtNode(
        mkOrder(accountSeller,
                wavesUsdPair,
                SELL,
                fullyAmountWaves,
                price,
                matcherFee = tooHighFee,
                version = version,
                matcherFeeAssetId = IssuedAsset(UsdId)))

      wavesNode1.api.balance(accountBuyer, Waves) should be(fullyAmountWaves)
      wavesNode1.api.balance(accountBuyer, IssuedAsset(UsdId)) shouldBe 0L
      wavesNode1.api.balance(accountSeller, Waves) should be(0L)
      wavesNode1.api.balance(accountSeller, IssuedAsset(UsdId)) shouldBe fullyAmountUsd

      dex1.api.reservedBalance(accountBuyer).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountBuyer).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(Waves, 0L) shouldBe 0L
      dex1.api.reservedBalance(accountSeller).getOrElse(IssuedAsset(UsdId), 0L) shouldBe 0L
    }

    s"buy order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api
        .tryPlace(
          mkOrder(
            createAccountWithBalance(fullyAmountUsd + minimalFee -> IssuedAsset(UsdId)),
            wavesUsdPair,
            BUY,
            fullyAmountWaves,
            price,
            tooLowFee,
            version = version,
            matcherFeeAssetId = IssuedAsset(UsdId)
          )) should failWith(9441542,
                             s"Required 2.52 ${UsdId.toString} as fee for this order, but given 2.51 ${UsdId.toString}")
    }

    s"sell order should be rejected if fee less then minimum possible fee when fee asset-type = $assetType" in {
      dex1.api
        .tryPlace(
          mkOrder(
            createAccountWithBalance(fullyAmountWaves -> Waves, minimalFee -> IssuedAsset(UsdId)),
            wavesUsdPair,
            SELL,
            fullyAmountWaves,
            price,
            tooLowFee,
            version = version,
            matcherFeeAssetId = IssuedAsset(UsdId)
          )) should failWith(9441542,
                             s"Required 2.52 ${UsdId.toString} as fee for this order, but given 2.51 ${UsdId.toString}")
    }
  }
}
