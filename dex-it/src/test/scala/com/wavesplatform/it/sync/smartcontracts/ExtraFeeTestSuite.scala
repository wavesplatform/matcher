package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class ExtraFeeTestSuite extends MatcherSuiteBase {

  override protected def suiteInitialDexConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex.order-fee {
       |  mode = dynamic
       |  dynamic.base-fee = $tradeFee
       |}
       """.stripMargin
    )

  private val trueScript  = Some(ExprScript(Terms.TRUE).explicitGet())
  private val falseScript = Some(ExprScript(Terms.FALSE).explicitGet())
  private val amount      = 1L
  private val price       = 100000000L

  private val asset0Tx = mkIssue(alice, "Asset0", defaultAssetQuantity, 0, smartIssueFee)
  private val asset0   = IssuedAsset(asset0Tx.id())

  private val asset1Tx = mkIssue(alice, "SmartAsset1", defaultAssetQuantity, 0, smartIssueFee, trueScript)
  private val asset1   = IssuedAsset(asset1Tx.id())

  private val asset2Tx = mkIssue(bob, "SmartAsset2", defaultAssetQuantity, 0, smartIssueFee, trueScript)
  private val asset2   = IssuedAsset(asset2Tx.id())

  private val assetWith2DecTx = mkIssue(bob, "SmartAsset3", defaultAssetQuantity, 2, smartIssueFee, trueScript)
  private val assetWith2Dec   = IssuedAsset(assetWith2DecTx.id())

  private val feeAssetTx = mkIssue(bob, "FeeSmartAsset", defaultAssetQuantity, 8, smartIssueFee, trueScript)
  private val feeAsset   = IssuedAsset(feeAssetTx.id())

  private val falseFeeAssetTx = mkIssue(bob, "FeeSmartAsset", defaultAssetQuantity, 8, smartIssueFee, falseScript)
  private val falseFeeAsset   = IssuedAsset(falseFeeAssetTx.id())

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(asset0Tx, asset1Tx, asset2Tx, assetWith2DecTx, feeAssetTx, falseFeeAssetTx, IssueBtcTx)
    broadcastAndAwait(
      mkTransfer(alice, bob, defaultAssetQuantity / 2, asset0, 0.005.waves),
      mkTransfer(alice, bob, defaultAssetQuantity / 2, asset1, 0.009.waves),
      mkTransfer(bob, alice, defaultAssetQuantity / 2, asset2, 0.005.waves)
    )
    broadcastAndAwait(mkSetAccountScriptText(alice, Some("true")))
  }

  "When matcher executes orders" - {
    "with one Smart Account and one Smart Asset" - {
      "then fee should be 0.003 + 0.004 (for Smart Asset only, not Smart Account)" in {
        val oneSmartPair = createAssetPair(asset0, asset1)

        val aliceInitBalance   = wavesNode1Api.balance(alice, Waves)
        val bobInitBalance     = wavesNode1Api.balance(bob, Waves)
        val matcherInitBalance = wavesNode1Api.balance(matcher, Waves)

        val expectedFee = tradeFee + smartFee // 1 x "smart asset"
        val invalidFee  = expectedFee - 1

        dex1Api.tryPlace(mkOrder(alice, oneSmartPair, SELL, amount, price, invalidFee, version = 2)) should failWith(
          9441542, // FeeNotEnough
          "Required 0.007 WAVES as fee for this order, but given 0.00699999 WAVES")

        placeAndAwait(mkOrder(alice, oneSmartPair, SELL, amount, price, expectedFee, version = 2))

        info("expected fee should be reserved")
        dex1Api.reservedBalance(alice)(Waves) shouldBe expectedFee

        val submitted = mkOrder(bob, oneSmartPair, BUY, amount, price, expectedFee, version = 2)
        dex1Api.place(submitted)
        waitForOrderAtNode(submitted)
        eventually {
          wavesNode1Api.balance(alice, Waves) shouldBe aliceInitBalance - expectedFee
          wavesNode1Api.balance(bob, Waves) shouldBe bobInitBalance - expectedFee
          wavesNode1Api.balance(matcher, Waves) shouldBe matcherInitBalance + expectedFee
        }
      }
    }

    "with one Smart Account, two Smart Assets and scripted Matcher" - {
      "then fee should be 0.003 + (0.004 * 2) + 0.004 (for Smart Assets and Matcher Script)" - {
        "and total fee should be divided proportionally with partial filling" in {
          broadcastAndAwait(mkSetAccountScriptText(matcher, Some("true")))

          val bothSmartPair = createAssetPair(asset1, asset2)

          val aliceInitBalance   = wavesNode1Api.balance(alice, Waves)
          val bobInitBalance     = wavesNode1Api.balance(bob, Waves)
          val matcherInitBalance = wavesNode1Api.balance(matcher, Waves)

          val expectedFee = tradeFee + 2 * smartFee + smartFee // 2 x "smart asset" and 1 x "matcher script"
          val invalidFee  = expectedFee - 1

          dex1Api.tryPlace(mkOrder(alice, bothSmartPair, SELL, amount, price, invalidFee, version = 2)) should failWith(
            9441542, // FeeNotEnough
            "Required 0.015 WAVES as fee for this order, but given 0.01499999 WAVES"
          )

          placeAndAwait(mkOrder(alice, bothSmartPair, SELL, amount, price, expectedFee, version = 2))

          info("expected fee should be reserved")
          dex1Api.reservedBalance(alice)(Waves) shouldBe expectedFee

          val submitted = mkOrder(bob, bothSmartPair, BUY, amount, price, expectedFee, version = 2)
          dex1Api.place(submitted)
          waitForOrderAtNode(submitted)
          eventually {
            wavesNode1Api.balance(alice, Waves) shouldBe aliceInitBalance - expectedFee
            wavesNode1Api.balance(bob, Waves) shouldBe bobInitBalance - expectedFee
            wavesNode1Api.balance(matcher, Waves) shouldBe matcherInitBalance + expectedFee
          }
        }
      }
    }

    "with non-waves asset fee with one Smart Account and one Smart Asset" in {
      val oneSmartPair = createAssetPair(asset0, asset1)

      val bobInitBalance     = wavesNode1Api.balance(bob, feeAsset)
      val matcherInitBalance = wavesNode1Api.balance(matcher, feeAsset)
      val feeAssetRate       = 0.0005

      dex1Api.upsertRate(feeAsset, feeAssetRate)
      dex1Api.upsertRate(btc, feeAssetRate)

      withClue("with same decimals count of assets in pair") {

        val expectedWavesFee = tradeFee + smartFee + smartFee // 1 x "smart asset" and 1 x "matcher script"
        val expectedFee      = 550L                           // 1 x "smart asset" and 1 x "matcher script"

        placeAndAwait(mkOrder(bob, oneSmartPair, SELL, amount, price, expectedFee, version = 3, matcherFeeAssetId = feeAsset))

        info("expected fee should be reserved")
        dex1Api.reservedBalance(bob)(feeAsset) shouldBe expectedFee

        val submitted = mkOrder(alice, oneSmartPair, BUY, amount, price, expectedWavesFee, version = 2)
        dex1Api.place(submitted)
        waitForOrderAtNode(submitted)

        eventually {
          wavesNode1Api.balance(bob, feeAsset) shouldBe (bobInitBalance - expectedFee)
          wavesNode1Api.balance(matcher, feeAsset) shouldBe (matcherInitBalance + expectedFee)
        }
      }

      withClue("with asset pair with different decimals count") {

        dex1Api.upsertRate(assetWith2Dec, 4)

        val wavesToAssetWith2DecPair = createAssetPair(assetWith2Dec, Waves)

        val bobWavesBalance   = wavesNode1Api.balance(bob, Waves)
        val bobAssetBalance   = wavesNode1Api.balance(bob, assetWith2Dec)
        val aliceWavesBalance = wavesNode1Api.balance(alice, Waves)
        val aliceAssetBalance = wavesNode1Api.balance(alice, assetWith2Dec)

        dex1Api.tryPlace(mkOrder(bob, wavesToAssetWith2DecPair, SELL, 10000L, 300.waves * 1000000L, 4, matcherFeeAssetId = assetWith2Dec)) should failWith(
          9441542, // FeeNotEnough
          s"Required 0.05 ${assetWith2DecTx.id()} as fee for this order, but given 0.04 ${assetWith2DecTx.id()}"
        )

        wavesNode1Api.balance(bob, Waves) shouldBe bobWavesBalance
        wavesNode1Api.balance(bob, assetWith2Dec) shouldBe bobAssetBalance
        wavesNode1Api.balance(alice, Waves) shouldBe aliceWavesBalance
        wavesNode1Api.balance(alice, assetWith2Dec) shouldBe aliceAssetBalance

        val bobOrder = mkOrder(bob, wavesToAssetWith2DecPair, SELL, 10000L, 300.waves * 1000000L, 5, matcherFeeAssetId = assetWith2Dec)
        dex1Api.place(bobOrder)
        dex1Api.reservedBalance(bob)(assetWith2Dec) shouldBe 10005L

        val aliceOrder = mkOrder(alice, wavesToAssetWith2DecPair, BUY, 20000L, 300.waves * 1000000L, 5, matcherFeeAssetId = assetWith2Dec)
        dex1Api.place(aliceOrder)
        waitForOrderAtNode(bobOrder)

        dex1Api.reservedBalance(alice)(Waves) shouldBe (300.waves * 100L)
        dex1Api.reservedBalance(bob) shouldBe Map()

        wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance + 300.waves * 100L)
        wavesNode1Api.balance(bob, assetWith2Dec) shouldBe (bobAssetBalance - 10005L)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 300.waves * 100L)
        wavesNode1Api.balance(alice, assetWith2Dec) shouldBe (aliceAssetBalance + 9998L)

        val anotherBobOrderId = mkOrder(bob, wavesToAssetWith2DecPair, SELL, 10000L, 300.waves * 1000000L, 5, assetWith2Dec)
        dex1Api.place(anotherBobOrderId)
        waitForOrderAtNode(anotherBobOrderId)

        dex1Api.reservedBalance(alice) shouldBe Map()
        wavesNode1Api.balance(bob, Waves) shouldBe (bobWavesBalance + 2 * 300.waves * 100L)
        wavesNode1Api.balance(bob, assetWith2Dec) shouldBe (bobAssetBalance - 2 * 10005L)
        wavesNode1Api.balance(alice, Waves) shouldBe (aliceWavesBalance - 2 * 300.waves * 100L)
        wavesNode1Api.balance(alice, assetWith2Dec) shouldBe (aliceAssetBalance + 2 * 9998L)
      }
    }
  }

  "with asset fee assigned false script" in {
    val oneSmartPair = createAssetPair(asset0, asset1)
    val feeAssetRate = 0.0005

    dex1Api.upsertRate(falseFeeAsset, feeAssetRate)

    dex1Api.tryPlace(mkOrder(bob, oneSmartPair, SELL, amount, price, 550, version = 3, matcherFeeAssetId = falseFeeAsset)) should failWith(
      11536130, // AssetScriptDeniedOrder
      s"The asset's script of ${falseFeeAssetTx.id()} rejected the order")
  }
}
