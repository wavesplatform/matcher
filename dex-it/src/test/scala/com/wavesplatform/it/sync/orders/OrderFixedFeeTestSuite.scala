package com.wavesplatform.it.sync.orders

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig.{matcher, _}
import com.wavesplatform.it.{MatcherSuiteBase, NTPTime}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

import scala.concurrent.duration._

class OrderFixedFeeTestSuite extends MatcherSuiteBase with NTPTime {

  import OrderFixedFeeTestSuite._

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
      decimals = 0,
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
      decimals = 0,
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
    "when has some asset as fixed fee in config and orders placed" - {
      "should accept orders if orders' matcherFeeAsset equal to specified in config" in {
        for (fixedAssetMatcherFee <- Seq(
          aliceAsset,
          aliceScriptedAsset
        )) {
          val fixedAssetMatcherFeeBase58 = fixedAssetMatcherFee.id.toString
          docker.restartNode(node,
            configWithOrderFeeFixed(
              matcherFeeAssetId = fixedAssetMatcherFeeBase58,
              minFee = minMatcherFee
            ))

          val someWavesPair = AssetPair(fixedAssetMatcherFee, Waves)
          node.broadcastTransfer(alice, bobAddress, node.assetBalance(aliceAddress, fixedAssetMatcherFeeBase58).balance / 2, minFee,
            Some(fixedAssetMatcherFeeBase58), waitForTx = true, feeAssetId = None)

          val ts = ntpTime.correctedTime()
          val expirationTimestamp = ts + Order.MaxLiveTime - 10000
          val orderAmount = 1
          val aliceAssetBalanceBefore = node.assetBalance(aliceAddress, fixedAssetMatcherFeeBase58).balance
          val bobAssetBalanceBefore = node.assetBalance(bobAddress, fixedAssetMatcherFeeBase58).balance
          val aliceWavesBalanceBefore = node.balanceDetails(aliceAddress).available
          val bobWavesBalanceBefore = node.balanceDetails(bobAddress).available

          val aliceOrderIdFill = node
            .placeOrder(
              Order
                .buy(
                  sender = alice,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = minMatcherFee,
                  version = 3,
                  matcherFeeAssetId = fixedAssetMatcherFee
                ))
            .message
            .id
          node.waitOrderStatus(someWavesPair, aliceOrderIdFill, "Accepted")
          node.reservedBalance(alice)("WAVES") shouldBe orderAmount * price / 100000000
          node.reservedBalance(alice)(s"$fixedAssetMatcherFeeBase58") shouldBe minMatcherFee - orderAmount
          node.tradableBalance(alice, someWavesPair)("WAVES") shouldBe aliceWavesBalanceBefore - (orderAmount * price / 100000000)
          node.tradableBalance(alice, someWavesPair)(s"$fixedAssetMatcherFeeBase58") shouldBe aliceAssetBalanceBefore - (minMatcherFee - orderAmount)

          val bobSellOrderId = node
            .placeOrder(
              Order
                .sell(
                  sender = bob,
                  matcher = matcherPublicKey,
                  pair = someWavesPair,
                  amount = orderAmount,
                  price = price,
                  timestamp = ts,
                  expiration = expirationTimestamp,
                  matcherFee = minMatcherFee,
                  version = 3,
                  matcherFeeAssetId = fixedAssetMatcherFee
                ))
            .message
            .id

          orderStatus(alice, someWavesPair, aliceOrderIdFill, "Filled")
          orderStatus(bob, someWavesPair, bobSellOrderId, "Filled")

          node.waitOrderInBlockchain(aliceOrderIdFill)

          node.assetBalance(aliceAddress, fixedAssetMatcherFeeBase58).balance shouldBe aliceAssetBalanceBefore - minMatcherFee + orderAmount
          node.assetBalance(bobAddress, fixedAssetMatcherFeeBase58).balance shouldBe bobAssetBalanceBefore - minMatcherFee - orderAmount
          node.balanceDetails(aliceAddress).available shouldBe aliceWavesBalanceBefore - (orderAmount * price / 100000000)
          node.balanceDetails(bobAddress).available shouldBe bobWavesBalanceBefore + (orderAmount * price / 100000000)
        }
      }

      "should accept orders if sender received amount > then fee amount" in {
        docker.restartNode(node,
          configWithOrderFeeFixed(
            matcherFeeAssetId = aliceAssetBase58,
            minFee = minMatcherFee))

        node.broadcastTransfer(
          bob,
          aliceAddress,
          node.assetBalance(bobAddress, aliceAssetBase58).balance,
          minFee,
          Some(aliceAssetBase58),
          None,
          waitForTx = true)

        val someWavesPair = AssetPair(aliceAsset, Waves)

        val ts = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime - 10000

        val bobOrderId = node
          .placeOrder(
            Order
              .buy(
                sender = bob,
                matcher = matcherPublicKey,
                pair = someWavesPair,
                amount = minMatcherFee,
                price = price,
                timestamp = ts,
                expiration = expirationTimestamp,
                matcherFee = minMatcherFee,
                version = 3,
                matcherFeeAssetId = aliceAsset
              ))
          .message
          .id

        node.waitOrderStatus(someWavesPair, bobOrderId, "Accepted")
      }

      "should reject orders if orders' matcherFeeAsset not equal to specified in config" in {
        val ts = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime
        val amount = 1
        val aliceWavesPair = AssetPair(aliceAsset, Waves)
        val buy = Order.buy(alice, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, Waves)
        val sell = Order.sell(bob, matcherPublicKey, aliceWavesPair, amount, price, ts, expirationTimestamp, minMatcherFee, version = 3, Waves)

        assertBadRequest(node.placeOrder(buy), "")
        assertBadRequest(node.placeOrder(sell), "")
      }

      "should reject orders if orders' matcherFee less than specified minFee in config" in {
        val ts = ntpTime.correctedTime()
        val expirationTimestamp = ts + Order.MaxLiveTime
        val amount = 1
        val aliceWavesPair = AssetPair(aliceAsset, Waves)
        val insufficientMatcherFee = minMatcherFee - 1
        val buy = Order.buy(alice,
          matcherPublicKey,
          aliceWavesPair,
          amount,
          price,
          ts,
          expirationTimestamp,
          insufficientMatcherFee,
          version = 3,
          aliceAsset)
        val sell = Order.sell(bob,
          matcherPublicKey,
          aliceWavesPair,
          amount,
          price,
          ts,
          expirationTimestamp,
          insufficientMatcherFee,
          version = 3,
          aliceAsset)

        assertBadRequestAndMessage(node.placeOrder(buy),
          f"Required $minMatcherFee $aliceAssetBase58 as fee for this order, but given $insufficientMatcherFee")
        assertBadRequestAndMessage(node.placeOrder(sell),
          f"Required $minMatcherFee $aliceAssetBase58 as fee for this order, but given $insufficientMatcherFee")

      }
    }
  }

  private def orderStatus(sender: KeyPair, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    node.waitOrderStatus(assetPair, orderId, expectedStatus, waitTime = 2.minutes)
}

object OrderFixedFeeTestSuite {
  def configWithOrderFeeFixed(matcherFeeAssetId: String = "WAVES", minFee: Long): Config = {
    parseString(
      s"""
         |waves.dex {
         | allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = fixed
         |
         |    fixed {
         |      asset = "$matcherFeeAssetId"
         |      min-fee = $minFee
         |   }
         |  }
         |}""".stripMargin)
  }
}
