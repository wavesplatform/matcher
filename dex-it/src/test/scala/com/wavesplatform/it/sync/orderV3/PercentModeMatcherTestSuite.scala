package com.wavesplatform.it.sync.orderV3

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.it.{MatcherSuiteBase, NTPTime}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import scala.concurrent.duration._

class PercentModeMatcherTestSuite extends MatcherSuiteBase with NTPTime {

  import PercentModeMatcherTestSuite._

  val minMatcherFee = 200000L
  val price = 100000000L
  val script: Script = ExprScript(Terms.TRUE).explicitGet()
  val scriptBase64: String = script.bytes.value.base64

  val aliceAssetBase58: String = node
    .broadcastIssue(
      alice,
      name = "AliceCoin",
      description = "AliceCoin for matcher's tests",
      quantity = 9999999999999L,
      decimals = 8,
      reissuable = false,
      fee = smartIssueFee,
      script = None,
      waitForTx = true
    )
    .id
  val aliceAsset = IssuedAsset(ByteStr.decodeBase58(aliceAssetBase58).get)
  val aliceScriptedAssetBase58: String = node
    .broadcastIssue(
      alice,
      name = "AliceSmartAsset",
      description = "AliceSmartAsset for matcher's tests",
      quantity = 9999999999999L,
      decimals = 8,
      reissuable = false,
      fee = smartIssueFee,
      script = Some(scriptBase64),
      waitForTx = true
    )
    .id
  val aliceScriptedAsset = IssuedAsset(ByteStr.decodeBase58(aliceScriptedAssetBase58).get)
  val aliceAddress = alice.publicKey.toAddress.stringRepr
  val bobAddress = bob.publicKey.toAddress.stringRepr

  val matcherPublicKey: PublicKey = matcher.publicKey

  "Matcher" - {
    "when has percent-mode for fee in config and orders placed" - {
      "should accept orders with amount/price/spending/receiving assets as matcherFeeAsset" in {
        val minFeePercent = 10
        for (percentAssetType <- Seq(
          "amount",
          "price",
          "spending",
          "receiving"
        )) {
          docker.restartNode(node,
            configWithOrderFeePercent(
              assetType = percentAssetType,
              minFeePercent = minFeePercent
            ))
          for (amountAsset <- Seq(
            aliceAsset,
            aliceScriptedAsset
          )) {
            val amountAssetBase58 = amountAsset.id.toString
            val someWavesPair = AssetPair(amountAsset, Waves)
            val ts = ntpTime.correctedTime()
            val expirationTimestamp = ts + Order.MaxLiveTime - 10000
            val amount = 100L
            val priceAssetSpending = 100L

            node.broadcastTransfer(alice, bobAddress, node.assetBalance(aliceAddress, amountAssetBase58).balance / 2, minFee, Some(amountAssetBase58), None, waitForTx = true)

            val (buyFeeAssetId, sellFeeAssetId, buyMatcherFee, sellMatcherFee) = percentAssetType match {
              case "amount" =>
                (amountAsset, amountAsset, amount * minFeePercent / 100, amount * minFeePercent / 100)
              case "price" =>
                (Waves, Waves, priceAssetSpending * minFeePercent / 100, priceAssetSpending * minFeePercent / 100)
              case "spending" =>
                (Waves, amountAsset, priceAssetSpending * minFeePercent / 100, amount * minFeePercent / 100)
              case "receiving" =>
                (amountAsset, Waves, amount * minFeePercent / 100, priceAssetSpending * minFeePercent / 100)
            }

            val buy =
              Order.buy(alice, matcherPublicKey, someWavesPair, amount, price, ts, expirationTimestamp, buyMatcherFee, version = 3, buyFeeAssetId)
            val sell =
              Order.sell(bob, matcherPublicKey, someWavesPair, amount, price, ts, expirationTimestamp, sellMatcherFee, version = 3, sellFeeAssetId)

            val aliceWavesBalanceBefore = node.balanceDetails(aliceAddress).available
            val bobWavesBalanceBefore = node.balanceDetails(bobAddress).available
            val aliceAssetBalanceBefore = node.assetBalance(aliceAddress, amountAssetBase58).balance
            val bobAssetBalanceBefore = node.assetBalance(bobAddress, amountAssetBase58).balance
            val aliceTradableBalanceBefore = node.tradableBalance(alice, someWavesPair)
            val bobTradableBalanceBefore = node.tradableBalance(bob, someWavesPair)

            val aliceOrderId = node
              .placeOrder(buy)
              .message
              .id

            node.waitOrderStatus(someWavesPair, aliceOrderId, "Accepted")

            percentAssetType match {
              case "amount" =>
                node.reservedBalance(alice) shouldBe Map("WAVES" -> 100)
                node.tradableBalance(alice, someWavesPair) shouldBe aliceTradableBalanceBefore.updated("WAVES",aliceTradableBalanceBefore("WAVES") - 100)
              case "price" =>
                node.reservedBalance(alice) shouldBe Map("WAVES" -> (amount + buyMatcherFee))
                node.tradableBalance(alice, someWavesPair) shouldBe aliceTradableBalanceBefore.updated("WAVES",aliceTradableBalanceBefore("WAVES") - 100 - buyMatcherFee)
              case "spending" =>
                node.reservedBalance(alice) shouldBe Map("WAVES" -> (amount + buyMatcherFee))
                node.tradableBalance(alice, someWavesPair) shouldBe aliceTradableBalanceBefore.updated("WAVES",aliceTradableBalanceBefore("WAVES") - 100 - buyMatcherFee)
              case "receiving" =>
                node.reservedBalance(alice) shouldBe Map("WAVES" -> amount)
                node.tradableBalance(alice, someWavesPair) shouldBe aliceTradableBalanceBefore.updated("WAVES",aliceTradableBalanceBefore("WAVES") - 100)
            }


            val bobOrderId = node
              .placeOrder(sell)
              .message
              .id

            orderStatus(alice, someWavesPair, aliceOrderId, "Filled")
            orderStatus(bob, someWavesPair, bobOrderId, "Filled")

            node.reservedBalance(alice) shouldBe Map.empty
            node.reservedBalance(bob) shouldBe Map.empty
//            node.tradableBalance(alice, someWavesPair) shouldBe aliceTradableBalanceBefore
//            node.tradableBalance(bob, someWavesPair) shouldBe bobTradableBalanceBefore

            node.waitOrderInBlockchain(aliceOrderId)

            percentAssetType match {
              case "amount" =>
                node.assetBalance(aliceAddress, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore - buyMatcherFee + amount
                node.assetBalance(bobAddress, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - sellMatcherFee - amount
                node.balanceDetails(aliceAddress).available shouldBe aliceWavesBalanceBefore - priceAssetSpending
                node.balanceDetails(bobAddress).available shouldBe bobWavesBalanceBefore + priceAssetSpending
                node.tradableBalance(alice, someWavesPair)("WAVES") shouldBe aliceTradableBalanceBefore("WAVES") - priceAssetSpending
                node.tradableBalance(bob, someWavesPair)("WAVES") shouldBe bobTradableBalanceBefore("WAVES") + priceAssetSpending
                node.tradableBalance(alice, someWavesPair)(amountAssetBase58) shouldBe aliceTradableBalanceBefore(amountAssetBase58) - buyMatcherFee + amount
                node.tradableBalance(bob, someWavesPair)(amountAssetBase58) shouldBe bobTradableBalanceBefore(amountAssetBase58) - sellMatcherFee - amount
              case "price" =>
                node.balanceDetails(aliceAddress).available shouldBe aliceWavesBalanceBefore - buyMatcherFee - priceAssetSpending
                node.balanceDetails(bobAddress).available shouldBe bobWavesBalanceBefore - sellMatcherFee + priceAssetSpending
                node.assetBalance(aliceAddress, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore + amount
                node.assetBalance(bobAddress, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - amount
              case "spending" =>
                node.balanceDetails(aliceAddress).available shouldBe aliceWavesBalanceBefore - buyMatcherFee - priceAssetSpending
                node.balanceDetails(bobAddress).available shouldBe bobWavesBalanceBefore + priceAssetSpending
                node.assetBalance(aliceAddress, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore + amount
                node.assetBalance(bobAddress, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - sellMatcherFee - amount
              case "receiving" =>
                node.balanceDetails(aliceAddress).available shouldBe aliceWavesBalanceBefore - priceAssetSpending
                node.balanceDetails(bobAddress).available shouldBe bobWavesBalanceBefore - sellMatcherFee + amount
                node.assetBalance(aliceAddress, amountAssetBase58).balance shouldBe aliceAssetBalanceBefore - buyMatcherFee + amount
                node.assetBalance(bobAddress, amountAssetBase58).balance shouldBe bobAssetBalanceBefore - amount

            }
          }
        }
      }

      "should reject orders if matcherFeeAsset not equal to amount/price/spending/receiving assets" in {
        val minFeePercent = 10
        for (percentAssetType <- Seq(
          "amount",
          "price",
          "spending",
          "receiving"
        )) {
          docker.restartNode(node,
            configWithOrderFeePercent(
              assetType = percentAssetType,
              minFeePercent = minFeePercent
            ))
          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount = 100
          val aliceWavesPair = AssetPair(aliceAsset, Waves)

          val (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset) = percentAssetType match {
            case "amount" =>
              val buy = Order.buy(alice,
                matcherPublicKey,
                aliceWavesPair,
                amount,
                price,
                ts,
                expirationTimestamp,
                amount * (minFeePercent / 100),
                version = 3,
                Waves)
              val sell = Order.sell(bob,
                matcherPublicKey,
                aliceWavesPair,
                amount,
                price,
                ts,
                expirationTimestamp,
                amount * (minFeePercent / 100),
                version = 3,
                Waves)
              val requiredBuyMatcherFeeAsset = aliceAssetBase58
              val requiredSellMatcherFeeAsset = aliceAssetBase58
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "price" =>
              val buy =
                Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, aliceAsset)
              val sell =
                Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, aliceAsset)
              val requiredBuyMatcherFeeAsset = "WAVES"
              val requiredSellMatcherFeeAsset = "WAVES"
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "spending" =>
              val buy =
                Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, aliceAsset)
              val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, Waves)
              val requiredBuyMatcherFeeAsset = "WAVES"
              val requiredSellMatcherFeeAsset = aliceAssetBase58
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
            case "receiving" =>
              val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, Waves)
              val sell =
                Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, aliceAsset)
              val requiredBuyMatcherFeeAsset = aliceAssetBase58
              val requiredSellMatcherFeeAsset = "WAVES"
              (buy, sell, requiredBuyMatcherFeeAsset, requiredSellMatcherFeeAsset)
          }

          assertBadRequestAndResponse(node
            .placeOrder(buy),
            f"Required one of the following fee asset: $requiredBuyMatcherFeeAsset")
          assertBadRequestAndResponse(node
            .placeOrder(sell),
            f"Required one of the following fee asset: $requiredSellMatcherFeeAsset")

        }
      }

      "should reject orders if orders' matcherFee amount less then specified in config" in {
        val minFeePercent = 10L
        for (percentAssetType <- Seq(
          "amount",
          "price",
          "spending",
          "receiving"
        )) {
          docker.restartNode(node,
            configWithOrderFeePercent(
              assetType = percentAssetType,
              minFeePercent = minFeePercent
            ))
          val someWavesPair = AssetPair(aliceAsset, Waves)
          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val amount = 100L
          val priceAssetSpending = 100L

          val (buyFeeAssetId, sellFeeAssetId, insufficientBuyMatcherFee, insufficientSellMatcherFee) = percentAssetType match {
            case "amount" =>
              (aliceAsset, aliceAsset, (amount * minFeePercent / 100) - 1, (amount * minFeePercent / 100) - 1)
            case "price" =>
              (Waves, Waves, (priceAssetSpending * minFeePercent / 100) - 1, (priceAssetSpending * minFeePercent / 100) - 1)
            case "spending" =>
              (Waves, aliceAsset, (priceAssetSpending * minFeePercent / 100) - 1, (amount * minFeePercent / 100) - 1)
            case "receiving" =>
              (aliceAsset, Waves, (amount * minFeePercent / 100) - 1, (priceAssetSpending * minFeePercent / 100) - 1)
          }


          assertBadRequestAndMessage(
            node.placeOrder(
              Order.buy(alice,
                matcherPublicKey,
                someWavesPair,
                amount,
                price,
                ts,
                expirationTimestamp,
                insufficientBuyMatcherFee,
                version = 3,
                buyFeeAssetId)),
            f"Required 0.0000001 ${buyFeeAssetId.maybeBase58Repr.getOrElse("WAVES")} as fee for this order, but given 0.00000009"
          )
          assertBadRequestAndMessage(
            node.placeOrder(
              Order.sell(bob,
                matcherPublicKey,
                someWavesPair,
                amount,
                price,
                ts,
                expirationTimestamp,
                insufficientSellMatcherFee,
                version = 3,
                sellFeeAssetId)),
            f"Required 0.0000001 ${sellFeeAssetId.maybeBase58Repr.getOrElse("WAVES")} as fee for this order, but given 0.00000009"
          )
        }
      }
    }
  }


  private def orderStatus(sender: KeyPair, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    node.waitOrderStatus(assetPair, orderId, expectedStatus, waitTime = 2.minutes)

}

object PercentModeMatcherTestSuite {

  def configWithOrderFeePercent(assetType: String, minFeePercent: Double): Config = {
    parseString(
      s"""
         |waves.dex {
         | allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = "percent"
         |
         |    percent {
         |        asset-type = "$assetType"
         |
         |        min-fee = "$minFeePercent"
         |      }
         |  }
         |}""".stripMargin)
  }
}
