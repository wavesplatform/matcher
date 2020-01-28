package com.wavesplatform.it.sync.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.it.test.Scripts
import com.wavesplatform.dex.it.waves.MkWavesEntities.IssueResults
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.config.DexTestConfig._

class ExtraFeeTestSuite extends MatcherSuiteBase {

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""waves.dex.order-fee.0 {
       |  mode = dynamic
       |  dynamic {
       |    base-maker-fee = $tradeFee
       |    base-taker-fee = $tradeFee
       |  }
       |}
       """.stripMargin
    )

  private val trueScript  = Option(Scripts.alwaysTrue)
  private val falseScript = Option(Scripts.alwaysFalse)
  private val amount      = 1L
  private val price       = 100000000L

  private val IssueResults(asset0Tx, _, asset0)     = mkIssueExtended(alice, "Asset0", defaultAssetQuantity, 0, smartIssueFee)
  private val IssueResults(asset1Tx, _, asset1)     = mkIssueExtended(alice, "SmartAsset1", defaultAssetQuantity, 0, smartIssueFee, trueScript)
  private val IssueResults(asset2Tx, _, asset2)     = mkIssueExtended(bob, "SmartAsset2", defaultAssetQuantity, 0, smartIssueFee, trueScript)
  private val IssueResults(feeAssetTx, _, feeAsset) = mkIssueExtended(bob, "FeeSmartAsset", defaultAssetQuantity, 8, smartIssueFee, trueScript)

  private val IssueResults(assetWith2DecTx, assetWith2DecId, assetWith2Dec) =
    mkIssueExtended(bob, "SmartAsset3", defaultAssetQuantity, 2, smartIssueFee, trueScript)

  private val IssueResults(falseFeeAssetTx, falseFeeAssetId, falseFeeAsset) =
    mkIssueExtended(bob, "FeeSmartAsset", defaultAssetQuantity, 8, smartIssueFee, falseScript)

  override protected def beforeAll(): Unit = {
    wavesNode1.start()

    broadcastAndAwait(asset0Tx, asset1Tx, asset2Tx, assetWith2DecTx, feeAssetTx, falseFeeAssetTx, IssueBtcTx)
    broadcastAndAwait(
      mkTransfer(alice, bob, defaultAssetQuantity / 2, asset0, 0.005.waves),
      mkTransfer(alice, bob, defaultAssetQuantity / 2, asset1, 0.009.waves),
      mkTransfer(bob, alice, defaultAssetQuantity / 2, asset2, 0.005.waves)
    )
    broadcastAndAwait(mkSetAccountScript(alice, trueScript))

    dex1.start()
  }

  "When matcher executes orders" - {
    "with one Smart Account and one Smart Asset" - {
      "then fee should be 0.003 + 0.004 (for Smart Asset only, not Smart Account)" in {
        val oneSmartPair = createAssetPair(asset0, asset1)

        val aliceInitBalance   = wavesNode1.api.balance(alice, Waves)
        val bobInitBalance     = wavesNode1.api.balance(bob, Waves)
        val matcherInitBalance = wavesNode1.api.balance(matcher, Waves)

        val expectedFee = tradeFee + smartFee // 1 x "smart asset"
        val invalidFee  = expectedFee - 1

        dex1.api.tryPlace(mkOrder(alice, oneSmartPair, SELL, amount, price, invalidFee, version = 2)) should failWith(
          9441542, // FeeNotEnough
          "Required 0.007 WAVES as fee for this order, but given 0.00699999 WAVES")

        placeAndAwaitAtDex(mkOrder(alice, oneSmartPair, SELL, amount, price, expectedFee, version = 2))

        info("expected fee should be reserved")
        dex1.api.reservedBalance(alice)(Waves) shouldBe expectedFee

        val submitted = mkOrder(bob, oneSmartPair, BUY, amount, price, expectedFee, version = 2)
        dex1.api.place(submitted)
        waitForOrderAtNode(submitted)

        eventually {
          wavesNode1.api.balance(alice, Waves) shouldBe aliceInitBalance - expectedFee
          wavesNode1.api.balance(bob, Waves) shouldBe bobInitBalance - expectedFee
          wavesNode1.api.balance(matcher, Waves) shouldBe matcherInitBalance + expectedFee
        }
      }
    }

    "with one Smart Account, two Smart Assets and scripted Matcher" - {
      "then fee should be 0.003 + (0.004 * 2) + 0.004 (for Smart Assets and Matcher Script)" - {
        "and total fee should be divided proportionally with partial filling" in {
          broadcastAndAwait(mkSetAccountScript(matcher, trueScript))

          dex1.restart() // matcher caches knowledge about it's script during start

          val bothSmartPair = createAssetPair(asset1, asset2)

          val aliceInitBalance   = wavesNode1.api.balance(alice, Waves)
          val bobInitBalance     = wavesNode1.api.balance(bob, Waves)
          val matcherInitBalance = wavesNode1.api.balance(matcher, Waves)

          val expectedFee = tradeFee + 2 * smartFee + smartFee // 2 x "smart asset" and 1 x "matcher script"
          val invalidFee  = expectedFee - 1

          dex1.api.tryPlace(mkOrder(alice, bothSmartPair, SELL, amount, price, invalidFee, version = 2)) should failWith(
            9441542, // FeeNotEnough
            "Required 0.015 WAVES as fee for this order, but given 0.01499999 WAVES"
          )

          placeAndAwaitAtDex(mkOrder(alice, bothSmartPair, SELL, amount, price, expectedFee, version = 2))

          info("expected fee should be reserved")
          dex1.api.reservedBalance(alice)(Waves) shouldBe expectedFee

          val submitted = mkOrder(bob, bothSmartPair, BUY, amount, price, expectedFee, version = 2)
          dex1.api.place(submitted)
          waitForOrderAtNode(submitted)

          eventually {
            wavesNode1.api.balance(alice, Waves) shouldBe aliceInitBalance - expectedFee
            wavesNode1.api.balance(bob, Waves) shouldBe bobInitBalance - expectedFee
            wavesNode1.api.balance(matcher, Waves) shouldBe matcherInitBalance + expectedFee
          }
        }
      }
    }

    "with non-waves asset fee with one Smart Account and one Smart Asset" in {
      val oneSmartPair = createAssetPair(asset0, asset1)

      val bobInitBalance     = wavesNode1.api.balance(bob, feeAsset)
      val matcherInitBalance = wavesNode1.api.balance(matcher, feeAsset)
      val feeAssetRate       = 0.0005

      dex1.api.upsertRate(feeAsset, feeAssetRate)
      dex1.api.upsertRate(btc, feeAssetRate)

      withClue("with same decimals count of assets in pair") {

        val expectedWavesFee = tradeFee + smartFee + smartFee // 1 x "smart asset" and 1 x "matcher script"
        val expectedFee      = 550L                           // 1 x "smart asset" and 1 x "matcher script"

        placeAndAwaitAtDex(mkOrder(bob, oneSmartPair, SELL, amount, price, expectedFee, version = 3, feeAsset = feeAsset))

        info("expected fee should be reserved")
        dex1.api.reservedBalance(bob)(feeAsset) shouldBe expectedFee

        val submitted = mkOrder(alice, oneSmartPair, BUY, amount, price, expectedWavesFee, version = 2)
        dex1.api.place(submitted)
        waitForOrderAtNode(submitted)

        eventually {
          wavesNode1.api.balance(bob, feeAsset) shouldBe (bobInitBalance - expectedFee)
          wavesNode1.api.balance(matcher, feeAsset) shouldBe (matcherInitBalance + expectedFee)
        }
      }

      withClue("with asset pair with different decimals count") {

        dex1.api.upsertRate(assetWith2Dec, 4)

        val asset2WithDecWavesPair = createAssetPair(assetWith2Dec, Waves)

        val bobWavesBalance   = wavesNode1.api.balance(bob, Waves)
        val bobAssetBalance   = wavesNode1.api.balance(bob, assetWith2Dec)
        val aliceWavesBalance = wavesNode1.api.balance(alice, Waves)
        val aliceAssetBalance = wavesNode1.api.balance(alice, assetWith2Dec)

        dex1.api.tryPlace(mkOrder(bob, asset2WithDecWavesPair, SELL, 10000L, 300.waves * 1000000L, 4, feeAsset = assetWith2Dec)) should failWith(
          9441542, // FeeNotEnough
          s"Required 0.05 $assetWith2DecId as fee for this order, but given 0.04 $assetWith2DecId"
        )

        wavesNode1.api.balance(bob, Waves) shouldBe bobWavesBalance
        wavesNode1.api.balance(bob, assetWith2Dec) shouldBe bobAssetBalance
        wavesNode1.api.balance(alice, Waves) shouldBe aliceWavesBalance
        wavesNode1.api.balance(alice, assetWith2Dec) shouldBe aliceAssetBalance

        val bobOrder = mkOrder(bob, asset2WithDecWavesPair, SELL, 10000L, 300.waves * 1000000L, 5, feeAsset = assetWith2Dec)
        dex1.api.place(bobOrder)
        dex1.api.reservedBalance(bob)(assetWith2Dec) shouldBe 10005L

        val aliceOrder = mkOrder(alice, asset2WithDecWavesPair, BUY, 20000L, 300.waves * 1000000L, 5, feeAsset = assetWith2Dec)
        dex1.api.place(aliceOrder)
        waitForOrderAtNode(bobOrder)

        dex1.api.reservedBalance(alice)(Waves) shouldBe (300.waves * 100L)
        dex1.api.reservedBalance(bob) shouldBe Map()

        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + 300.waves * 100L)
        wavesNode1.api.balance(bob, assetWith2Dec) shouldBe (bobAssetBalance - 10005L)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 300.waves * 100L)
        wavesNode1.api.balance(alice, assetWith2Dec) shouldBe (aliceAssetBalance + 9998L)

        val anotherBobOrderId = mkOrder(bob, asset2WithDecWavesPair, SELL, 10000L, 300.waves * 1000000L, 5, assetWith2Dec)
        dex1.api.place(anotherBobOrderId)
        waitForOrderAtNode(anotherBobOrderId)

        dex1.api.reservedBalance(alice) shouldBe Map()
        wavesNode1.api.balance(bob, Waves) shouldBe (bobWavesBalance + 2 * 300.waves * 100L)
        wavesNode1.api.balance(bob, assetWith2Dec) shouldBe (bobAssetBalance - 2 * 10005L)
        wavesNode1.api.balance(alice, Waves) shouldBe (aliceWavesBalance - 2 * 300.waves * 100L)
        wavesNode1.api.balance(alice, assetWith2Dec) shouldBe (aliceAssetBalance + 2 * 9998L)
      }
    }
  }

  "with asset fee assigned false script" in {
    val oneSmartPair = createAssetPair(asset0, asset1)
    val feeAssetRate = 0.0005

    dex1.api.upsertRate(falseFeeAsset, feeAssetRate)

    dex1.api.tryPlace(mkOrder(bob, oneSmartPair, SELL, amount, price, 550, version = 3, feeAsset = falseFeeAsset)) should failWith(
      11536130, // AssetScriptDeniedOrder
      s"The asset's script of $falseFeeAssetId rejected the order")
  }
}
