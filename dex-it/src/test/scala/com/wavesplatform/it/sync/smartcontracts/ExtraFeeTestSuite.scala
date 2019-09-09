package com.wavesplatform.it.sync.smartcontracts

import com.softwaremill.sttp.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NewMatcherSuiteBase
import com.wavesplatform.it.api.OrderStatus
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.sync.{smartFee, smartIssueFee, tradeFee}
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}

class ExtraFeeTestSuite extends NewMatcherSuiteBase {

  override protected def suiteInitialDexConfig: Config = ConfigFactory.parseString(
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

  private val feeAssetTx = mkIssue(bob, "FeeSmartAsset", defaultAssetQuantity, 8, smartIssueFee, trueScript)
  private val feeAsset   = IssuedAsset(feeAssetTx.id())

  private val falseFeeAssetTx = mkIssue(bob, "FeeSmartAsset", defaultAssetQuantity, 8, smartIssueFee, falseScript)
  private val falseFeeAsset   = IssuedAsset(falseFeeAssetTx.id())

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    broadcastAndAwait(asset0Tx, asset1Tx, asset2Tx, feeAssetTx, falseFeeAssetTx, IssueBtcTx)
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
          9441542,
          "Required 0.007 WAVES as fee for this order, but given 0.00699999 WAVES")

        val counter = mkOrder(alice, oneSmartPair, SELL, amount, price, expectedFee, version = 2)
        dex1Api.place(counter)
        dex1Api.waitForOrderStatus(counter, OrderStatus.Accepted)

        info("expected fee should be reserved")
        dex1Api.reservedBalance(alice)(Waves) shouldBe expectedFee

        val submitted = mkOrder(bob, oneSmartPair, BUY, amount, price, expectedFee, version = 2)
        dex1Api.place(submitted)
        waitForOrderAtNode(submitted.id())

        wavesNode1Api.balance(alice, Waves) shouldBe aliceInitBalance - expectedFee
        wavesNode1Api.balance(bob, Waves) shouldBe bobInitBalance - expectedFee
        wavesNode1Api.balance(matcher, Waves) shouldBe matcherInitBalance + expectedFee
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
            9441542,
            "Required 0.015 WAVES as fee for this order, but given 0.01499999 WAVES"
          )

          val counter = mkOrder(alice, bothSmartPair, SELL, amount, price, expectedFee, version = 2)
          dex1Api.place(counter)
          dex1Api.waitForOrderStatus(counter, OrderStatus.Accepted)

          info("expected fee should be reserved")
          dex1Api.reservedBalance(alice)(Waves) shouldBe expectedFee

          val submitted = mkOrder(bob, bothSmartPair, BUY, amount, price, expectedFee, version = 2)
          dex1Api.place(submitted)
          waitForOrderAtNode(submitted.id())

          wavesNode1Api.balance(alice, Waves) shouldBe aliceInitBalance - expectedFee
          wavesNode1Api.balance(bob, Waves) shouldBe bobInitBalance - expectedFee
          wavesNode1Api.balance(matcher, Waves) shouldBe matcherInitBalance + expectedFee
        }
      }
    }

    "with non-waves asset fee with one Smart Account and one Smart Asset" in {
      val oneSmartPair = createAssetPair(asset0, asset1)

      val bobInitBalance     = wavesNode1Api.balance(bob, feeAsset)
      val matcherInitBalance = wavesNode1Api.balance(matcher, feeAsset)
      val feeAssetRate       = 0.0005
      dex1Api.upsertRate(feeAsset, feeAssetRate)._1 shouldBe StatusCodes.Created
      dex1Api.upsertRate(btc, feeAssetRate)._1 shouldBe StatusCodes.Created

      val expectedWavesFee = tradeFee + smartFee + smartFee // 1 x "smart asset" and 1 x "matcher script"
      val expectedFee      = 550L // 1 x "smart asset" and 1 x "matcher script"
      val counter          = mkOrder(bob, oneSmartPair, SELL, amount, price, expectedFee, version = 3, matcherFeeAssetId = feeAsset)
      dex1Api.place(counter)
      dex1Api.waitForOrderStatus(counter, OrderStatus.Accepted)

      info("expected fee should be reserved")
      dex1Api.reservedBalance(bob)(feeAsset) shouldBe expectedFee

      val submitted = mkOrder(alice, oneSmartPair, BUY, amount, price, expectedWavesFee, version = 2)
      dex1Api.place(submitted)
      waitForOrderAtNode(submitted.id())

      wavesNode1Api.balance(bob, feeAsset) shouldBe (bobInitBalance - expectedFee)
      wavesNode1Api.balance(matcher, feeAsset) shouldBe (matcherInitBalance + expectedFee)
    }

    "with asset fee assigned false script" in {
      val oneSmartPair = createAssetPair(asset0, asset1)
      val feeAssetRate = 0.0005
      dex1Api.upsertRate(falseFeeAsset, feeAssetRate)._1 shouldBe StatusCodes.Created
      dex1Api.tryPlace(mkOrder(bob, oneSmartPair, SELL, amount, price, 550, version = 3, matcherFeeAssetId = falseFeeAsset)) should failWith(
        11536130,
        s"The asset's script of ${falseFeeAssetTx.id()} rejected the order")
    }
  }

}
