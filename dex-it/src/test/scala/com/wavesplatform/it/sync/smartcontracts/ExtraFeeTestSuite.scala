package com.wavesplatform.it.sync.smartcontracts

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, Base64, EitherExt2}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lang.directives.values.{Expression, V1}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.lang.directives.values.{Expression, V1}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.extensions.Distributions.Empty
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import fastparse.core.Parsed

class ExtraFeeTestSuite extends MatcherSuiteBase {

  private def createBoolScript(code: String) = {
    val Parsed.Success(expr, _) = Parser.parseExpr(code).get
    ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), expr).explicitGet()._1).explicitGet()
  }

  val trueScript = Some(Base64.encode(createBoolScript("true").bytes.apply)) //TODO добавить типовые проверки в скрипт
  val falseScript = Some(Base64.encode(createBoolScript("false").bytes.apply))
  val amount      = 1L
  val price       = 100000000L

  // set smart account
  setContract(Some("true"), alice)

  // issue one simple and three smart assets
  val asset0: String = node
    .broadcastIssue(alice, "Asset0", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, None)
    .id
  val asset1: String = node
    .broadcastIssue(alice, "SmartAsset1", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, trueScript)
    .id
  val asset2: String = node
    .broadcastIssue(bob, "SmartAsset2", "Test", defaultAssetQuantity, 0, reissuable = false, smartIssueFee, trueScript)
    .id
  val assetWith2Dec: String = node
    .broadcastIssue(bob, "SmartAsset3", "Smart asset with 2 decimals", defaultAssetQuantity, 2, reissuable = false, smartIssueFee, trueScript)
    .id
  val feeAsset: ByteStr = ByteStr(Base58.decode(node
    .broadcastIssue(bob, "FeeSmartAsset", "Test", defaultAssetQuantity, 8, reissuable = false, smartIssueFee, trueScript)
    .id))
  val falseFeeAsset: ByteStr = ByteStr(Base58.decode(node
    .broadcastIssue(bob, "FeeSmartAsset", "Test", defaultAssetQuantity, 8, reissuable = false, smartIssueFee, falseScript)
    .id))
  Seq(asset0, asset1, asset2, feeAsset.toString).foreach(node.waitForTransaction(_))

  // distribute
  {
    val xs = Seq(
      node.broadcastTransfer(alice, bob.toAddress.toString, defaultAssetQuantity / 2, 0.005.waves, Some(asset0), None).id,
      node.broadcastTransfer(alice, bob.toAddress.toString, defaultAssetQuantity / 2, 0.009.waves, Some(asset1), None).id,
      node.broadcastTransfer(bob, alice.toAddress.toString, defaultAssetQuantity / 2, 0.005.waves, Some(asset2), None).id
    )
    xs.foreach(node.waitForTransaction(_))

    val txIds = Seq(IssueBtcTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))
  }

  override protected def nodeConfigs: Seq[Config] = {

    val orderFeeSettingsStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = dynamic
         |    dynamic {
         |      base-fee = $tradeFee
         |    }
         |  }
         |}
       """.stripMargin

    super.nodeConfigs.map(
      ConfigFactory
        .parseString(orderFeeSettingsStr)
        .withFallback
    )
  }

  "When matcher executes orders" - {
    "with one Smart Account and one Smart Asset" - {
      "then fee should be 0.003 + 0.004 (for Smart Asset only, not Smart Account)" in {
        val oneSmartPair = createAssetPair(asset0, asset1)

        val aliceInitBalance = node.accountBalances(alice.toAddress.toString)._1
        val bobInitBalance = node.accountBalances(bob.toAddress.toString)._1
        val matcherInitBalance = node.accountBalances(matcher.toAddress.toString)._1

        val expectedFee = tradeFee + smartFee // 1 x "smart asset"
        val invalidFee = expectedFee - 1

        node.expectRejectedOrderPlacement(
          alice,
          oneSmartPair,
          SELL,
          amount,
          price,
          invalidFee,
          2,
          expectedMessage = Some("Required 0.007 WAVES as fee for this order, but given 0.00699999 WAVES")
        )

        val counter = node.placeOrder(alice, oneSmartPair, SELL, amount, price, expectedFee, 2).message.id
        node.waitOrderStatus(oneSmartPair, counter, "Accepted")

        info("expected fee should be reserved")
        node.reservedBalance(alice)("WAVES") shouldBe expectedFee

        val submitted = node.placeOrder(bob, oneSmartPair, BUY, amount, price, expectedFee, 2).message.id
        node.waitOrderInBlockchain(submitted)

        node.accountBalances(alice.toAddress.toString)._1 shouldBe aliceInitBalance - expectedFee
        node.accountBalances(bob.toAddress.toString)._1 shouldBe bobInitBalance - expectedFee
        node.accountBalances(matcher.toAddress.toString)._1 shouldBe matcherInitBalance + expectedFee
      }
    }

    "with one Smart Account, two Smart Assets and scripted Matcher" - {
      "then fee should be 0.003 + (0.004 * 2) + 0.004 (for Smart Assets and Matcher Script)" - {
        "and total fee should be divided proportionally with partial filling" in {
          setContract(Some("true"), matcher)

          val bothSmartPair = createAssetPair(asset1, asset2)

          val aliceInitBalance = node.accountBalances(alice.toAddress.toString)._1
          val bobInitBalance = node.accountBalances(bob.toAddress.toString)._1
          val matcherInitBalance = node.accountBalances(matcher.toAddress.toString)._1

          val expectedFee = tradeFee + 2 * smartFee + smartFee // 2 x "smart asset" and 1 x "matcher script"
          val invalidFee = expectedFee - 1

          node.expectRejectedOrderPlacement(
            alice,
            bothSmartPair,
            SELL,
            amount,
            price,
            invalidFee,
            2,
            expectedMessage = Some("Required 0.015 WAVES as fee for this order, but given 0.01499999 WAVES")
          )

          val counter = node.placeOrder(alice, bothSmartPair, SELL, amount, price, expectedFee, 2).message.id
          node.waitOrderStatus(bothSmartPair, counter, "Accepted")

          info("expected fee should be reserved")
          node.reservedBalance(alice)("WAVES") shouldBe expectedFee

          val submitted = node.placeOrder(bob, bothSmartPair, BUY, amount, price, expectedFee, 2).message.id
          node.waitOrderInBlockchain(submitted)

          node.accountBalances(alice.toAddress.toString)._1 shouldBe aliceInitBalance - expectedFee
          node.accountBalances(bob.toAddress.toString)._1 shouldBe bobInitBalance - expectedFee
          node.accountBalances(matcher.toAddress.toString)._1 shouldBe matcherInitBalance + expectedFee
        }
      }
    }

    "with non-waves asset fee with one Smart Account and one Smart Asset" in {
      val oneSmartPair = createAssetPair(asset0, asset1)

      val bobInitBalance     = node.assetBalance(bob.toAddress.toString, feeAsset.toString).balance
      val matcherInitBalance = node.assetBalance(matcher.toAddress.toString, feeAsset.toString).balance
      val feeAssetRate       = 0.0005
      node.upsertRate(IssuedAsset(feeAsset), feeAssetRate, expectedStatusCode = StatusCodes.Created)
      node.upsertRate(IssuedAsset(BtcId), feeAssetRate, expectedStatusCode = StatusCodes.Created)

      withClue("with same decimals count of assets in pair") {
        val expectedWavesFee = tradeFee + smartFee + smartFee // 1 x "smart asset" and 1 x "matcher script"
        val expectedFee      = 550L                           // 1 x "smart asset" and 1 x "matcher script"
        val counter = node
          .placeOrder(
            sender = bob,
            pair = oneSmartPair,
            orderType = SELL,
            amount = amount,
            price = price,
            fee = expectedFee,
            version = 3,
            feeAsset = IssuedAsset(feeAsset)
          )
          .message
          .id
        node.waitOrderStatus(oneSmartPair, counter, "Accepted")

        info("expected fee should be reserved")
        node.reservedBalance(bob)(feeAsset.toString) shouldBe expectedFee

        val submitted = node.placeOrder(alice, oneSmartPair, BUY, amount, price, expectedWavesFee, 2).message.id
        node.waitOrderInBlockchain(submitted)

        node.assertAssetBalance(bob.toAddress.toString, feeAsset.toString, bobInitBalance - expectedFee)
        node.assertAssetBalance(matcher.toAddress.toString, feeAsset.toString, matcherInitBalance + expectedFee)
      }

      node.upsertRate(IssuedAsset(ByteStr(Base58.decode(assetWith2Dec))), 4, expectedStatusCode = StatusCodes.Created)
      withClue("with asset pair with different decimals count") {
        val wavesToAssetWith2DecPair = createAssetPair(assetWith2Dec, "WAVES")
        val bobWavesBalance          = node.accountBalances(bob.toAddress.toString)._1
        val bobAssetBalance          = node.assetBalance(bob.toAddress.toString, assetWith2Dec).balance
        val aliceWavesBalance        = node.accountBalances(alice.toAddress.toString)._1
        val aliceAssetBalance        = node.assetBalance(alice.toAddress.toString, assetWith2Dec).balance

        assertBadRequestAndMessage(
          node.placeOrder(
            sender = bob,
            pair = wavesToAssetWith2DecPair,
            orderType = SELL,
            amount = 10000L,
            price = 300.waves * 1000000L,
            fee = 4,
            version = 3,
            feeAsset = IssuedAsset(ByteStr(Base58.decode(assetWith2Dec)))
          ),
          s"Required 0.05 $assetWith2Dec as fee for this order, but given 0.04 $assetWith2Dec"
        )

        node.accountBalances(bob.toAddress.toString)._1 shouldBe bobWavesBalance
        node.assetBalance(bob.toAddress.toString, assetWith2Dec).balance shouldBe bobAssetBalance
        node.accountBalances(alice.toAddress.toString)._1 shouldBe aliceWavesBalance
        node.assetBalance(alice.toAddress.toString, assetWith2Dec).balance shouldBe aliceAssetBalance

        val bobOrderId = node
          .placeOrder(
            sender = bob,
            pair = wavesToAssetWith2DecPair,
            orderType = SELL,
            amount = 10000L,
            price = 300.waves * 1000000L,
            fee = 5,
            version = 3,
            feeAsset = IssuedAsset(ByteStr(Base58.decode(assetWith2Dec)))
          )
          .message
          .id
        node.reservedBalance(bob) shouldBe Map(assetWith2Dec.toString -> 10005L)

        node
          .placeOrder(
            sender = alice,
            pair = wavesToAssetWith2DecPair,
            orderType = BUY,
            amount = 20000L,
            price = 300.waves * 1000000L,
            fee = 5,
            version = 3,
            feeAsset = IssuedAsset(ByteStr(Base58.decode(assetWith2Dec)))
          )
          .message
          .id
        node.waitOrderInBlockchain(bobOrderId)

        node.reservedBalance(alice) shouldBe Map("WAVES" -> 300.waves * 100L)
        node.reservedBalance(bob) shouldBe Map()
        node.accountBalances(bob.toAddress.toString)._1 shouldBe bobWavesBalance + 300.waves * 100L
        node.assetBalance(bob.toAddress.toString, assetWith2Dec).balance shouldBe bobAssetBalance - 10005L
        node.accountBalances(alice.toAddress.toString)._1 shouldBe aliceWavesBalance - 300.waves * 100L
        node.assetBalance(alice.toAddress.toString, assetWith2Dec).balance shouldBe aliceAssetBalance + 9998L

        val anotherBobOrderId = node
          .placeOrder(
            sender = bob,
            pair = wavesToAssetWith2DecPair,
            orderType = SELL,
            amount = 10000L,
            price = 300.waves * 1000000L,
            fee = 5,
            version = 3,
            feeAsset = IssuedAsset(ByteStr(Base58.decode(assetWith2Dec)))
          )
          .message
          .id
        node.waitOrderInBlockchain(anotherBobOrderId)

        node.reservedBalance(alice) shouldBe Map()
        node.accountBalances(bob.toAddress.toString)._1 shouldBe bobWavesBalance + 2 * 300.waves * 100L
        node.assetBalance(bob.toAddress.toString, assetWith2Dec).balance shouldBe bobAssetBalance - 2 * 10005L
        node.accountBalances(alice.toAddress.toString)._1 shouldBe aliceWavesBalance - 2 * 300.waves * 100L
        node.assetBalance(alice.toAddress.toString, assetWith2Dec).balance shouldBe aliceAssetBalance + 2 * 9998L
      }
    }

    "with asset fee assigned false script" in {
      val oneSmartPair = createAssetPair(asset0, asset1)
      val feeAssetRate = 0.0005
      node.upsertRate(IssuedAsset(falseFeeAsset), feeAssetRate, expectedStatusCode = StatusCodes.Created)
      assertBadRequestAndResponse(
        node.placeOrder(
          sender = bob,
          pair = oneSmartPair,
          orderType = SELL,
          amount = amount,
          price = price,
          fee = 550,
          version = 3,
          feeAsset = IssuedAsset(falseFeeAsset)
        ),
        s"The asset's script of $falseFeeAsset rejected the order"
      )
    }
  }
}
