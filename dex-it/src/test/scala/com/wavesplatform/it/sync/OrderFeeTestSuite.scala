package com.wavesplatform.it.sync

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.it.MatcherSuiteBase
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.config.DexTestConfig._
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.OrderType

import scala.concurrent.duration._

class OrderFeeTestSuite extends MatcherSuiteBase {

  val baseFee = 300000

  override protected def nodeConfigs: Seq[Config] = {

    val orderFeeSettingsStr =
      s"""
         |waves.dex {
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee {
         |    mode = dynamic
         |    dynamic {
         |      base-fee = $baseFee
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

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val txIds = Seq(IssueUsdTx, IssueEthTx, IssueBtcTx).map(_.json()).map(node.broadcastRequest(_).id)
    txIds.foreach(node.waitForTransaction(_))
  }

  "supported non-waves order fee" - {
    val btcRate = 0.0005
    val ethRate = 0.0064

    "is not enough" in {
      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))

      assertBadRequestAndResponse(
        node.placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 100L,
          version = 3: Byte,
          matcherFeeAssetId = IssuedAsset(BtcId)
        ),
        s"Required 0.0000015 $BtcId as fee for this order, but given 0.000001 $BtcId"
      )

      assertBadRequestAndResponse(
        node.placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 1920L,
          version = 3: Byte,
          matcherFeeAssetId = IssuedAsset(EthId)
        ),
        s"Not enough tradable balance. The order requires 0.0000192 $EthId and 0.0005 $BtcId"
      )
      Array(BtcId, EthId)
        .foreach(assetId => node.deleteRate(IssuedAsset(assetId)))
    }

    "is enough" in {
      node.upsertRate(IssuedAsset(BtcId), btcRate, expectedStatusCode = StatusCodes.Created)
      node.placeOrder(
        sender = bob,
        pair = wavesBtcPair,
        orderType = OrderType.SELL,
        amount = 1.waves,
        price = 50000L,
        fee = 150L,
        version = 3: Byte,
        matcherFeeAssetId = IssuedAsset(BtcId)
      )
      node.reservedBalance(bob).keys shouldNot contain(BtcId.toString)
      node.reservedBalance(bob)("WAVES") shouldEqual 100000000L
      node.cancelAllOrders(bob)

      node.placeOrder(
        sender = bob,
        pair = wavesBtcPair,
        orderType = OrderType.BUY,
        amount = 1.waves,
        price = 50000L,
        fee = 150L,
        version = 3: Byte,
        matcherFeeAssetId = IssuedAsset(BtcId)
      )
      node.reservedBalance(bob)(BtcId.toString) shouldEqual 50150L
      node.reservedBalance(bob).keys shouldNot contain("WAVES")
      node.cancelAllOrders(bob)
      node.deleteRate(IssuedAsset(BtcId))
    }

    "missing part of fee can be withdraw after order fill" in {
      node.upsertRate(IssuedAsset(EthId), ethRate, expectedStatusCode = StatusCodes.Created)
      val bobEthBalance = node.assetBalance(bob.toAddress.toString, EthId.toString).balance
      if (bobEthBalance > 0) {
        node.broadcastTransfer(bob, alice.toAddress.toString, bobEthBalance, minFee, Option(EthId.toString), None, waitForTx = true)
      }
      val bobOrderId = node
        .placeOrder(
          sender = bob,
          pair = ethWavesPair,
          orderType = OrderType.BUY,
          amount = 100000000L,
          price = 156250000000L,
          fee = 1920L,
          version = 3: Byte,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id
      node.placeOrder(
        sender = alice,
        pair = ethWavesPair,
        orderType = OrderType.SELL,
        amount = 100000000L,
        price = 156250000000L,
        fee = 1920L,
        version = 3: Byte,
        matcherFeeAssetId = IssuedAsset(EthId)
      )
      node.waitOrderInBlockchain(bobOrderId)
      node.assertAssetBalance(bob.toAddress.toString, EthId.toString, 100000000L - 1920L)
      node.deleteRate(IssuedAsset(EthId))
    }
  }

  "asset fee is not supported" - {
    val btcRate = 0.0005
    val ethRate = 0.0064
    val order = node.prepareOrder(
      sender = bob,
      pair = wavesBtcPair,
      orderType = OrderType.BUY,
      amount = 1.waves,
      price = 50000L,
      fee = 150L,
      version = 3: Byte,
      matcherFeeAssetId = IssuedAsset(BtcId)
    )

    "only waves supported" in {
      assertBadRequestAndResponse(node.placeOrder(order), s"Required one of the following fee asset: WAVES. But given $BtcId")
    }

    "not only waves supported" in {
      node.upsertRate(IssuedAsset(EthId), 0.1, expectedStatusCode = StatusCodes.Created)
      assertBadRequestAndResponse(node.placeOrder(order), s"Required one of the following fee asset: $EthId, WAVES. But given $BtcId")
      node.deleteRate(IssuedAsset(EthId))
    }

    "asset became not supported after order was placed" in {
      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
      val bobBtcBalance   = node.assetBalance(bob.toAddress.toString, BtcId.toString).balance
      val aliceBtcBalance = node.assetBalance(alice.toAddress.toString, BtcId.toString).balance
      val aliceEthBalance = node.assetBalance(alice.toAddress.toString, EthId.toString).balance
      val bobOrderId      = node.placeOrder(order).message.id
      node.deleteRate(IssuedAsset(BtcId))
      node
        .placeOrder(
          sender = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          fee = 1920L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id
      node.waitOrderStatus(wavesBtcPair, bobOrderId, "Filled", 500.millis)
      node.waitOrderInBlockchain(bobOrderId)
      node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 150L - 50000L)
      node.assertAssetBalance(alice.toAddress.toString, BtcId.toString, aliceBtcBalance + 50000L)
      node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - 1920L)
      node.deleteRate(IssuedAsset(EthId))
    }

    "asset became not supported after order was partially filled" in {
      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
      val bobBtcBalance   = node.assetBalance(bob.toAddress.toString, BtcId.toString).balance
      val aliceBtcBalance = node.assetBalance(alice.toAddress.toString, BtcId.toString).balance
      val aliceEthBalance = node.assetBalance(alice.toAddress.toString, EthId.toString).balance
      val aliceOrderId = node
        .placeOrder(
          sender = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 2.waves,
          price = 50000L,
          fee = 1920L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id
      node.reservedBalance(alice)(EthId.toString) shouldBe 1920L
      node.placeOrder(
        sender = bob,
        pair = wavesBtcPair,
        orderType = OrderType.BUY,
        amount = 1.waves,
        price = 50000L,
        fee = 150L,
        version = 3: Byte,
        matcherFeeAssetId = IssuedAsset(BtcId)
      )
      node.waitOrderStatus(wavesBtcPair, aliceOrderId, "PartiallyFilled", 500.millis)
      node.waitOrderInBlockchain(aliceOrderId)
      node.reservedBalance(alice)(EthId.toString) shouldBe 960L
      node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 150L - 50000L)
      node.assertAssetBalance(alice.toAddress.toString, BtcId.toString, aliceBtcBalance + 50000L)
      node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - 960L)
      node.deleteRate(IssuedAsset(EthId))
      val bobSecondOrderId = node
        .placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 150L,
          version = 3: Byte,
          matcherFeeAssetId = IssuedAsset(BtcId)
        )
        .message
        .id
      node.waitOrderStatus(wavesBtcPair, aliceOrderId, "Filled", 500.millis)
      node.waitOrderInBlockchain(bobSecondOrderId)
      node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 300L - 100000L)
      node.assertAssetBalance(alice.toAddress.toString, BtcId.toString, aliceBtcBalance + 100000L)
      node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - 1920L)
      node.deleteRate(IssuedAsset(BtcId))
    }

    "rates of asset pair was changed while order is placed" in {
      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
      val bobBtcBalance = node.assetBalance(bob.toAddress.toString, BtcId.toString).balance
      val bobOrderId = node
        .placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 150L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(BtcId)
        )
        .message
        .id
      val newBtcRate = btcRate * 2
      node.upsertRate(IssuedAsset(BtcId), newBtcRate, expectedStatusCode = StatusCodes.OK)
      node.reservedBalance(bob)(BtcId.toString) shouldBe 50150L
      node
        .placeOrder(
          sender = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          fee = 1920L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id
      node.waitOrderInBlockchain(bobOrderId)
      node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 50150L)
      Array(BtcId, EthId).foreach(assetId => node.deleteRate(IssuedAsset(assetId)))
    }
  }

  "orders with non-waves asset fee" - {
    val btcRate = 0.0005
    val ethRate = 0.0064

    "are full filled" in {
      val bobBtcBalance     = node.assetBalance(bob.toAddress.toString, BtcId.toString).balance
      val aliceBtcBalance   = node.assetBalance(alice.toAddress.toString, BtcId.toString).balance
      val aliceEthBalance   = node.assetBalance(alice.toAddress.toString, EthId.toString).balance
      val matcherEthBalance = node.assetBalance(matcher.toAddress.toString, EthId.toString).balance
      val matcherBtcBalance = node.assetBalance(matcher.toAddress.toString, BtcId.toString).balance
      val bobWavesBalance   = node.accountBalances(bob.toAddress.toString)._1
      val aliceWavesBalance = node.accountBalances(alice.toAddress.toString)._1

      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
      val bobOrderId = node
        .placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 150L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(BtcId)
        )
        .message
        .id
      node.waitOrderStatus(wavesBtcPair, bobOrderId, "Accepted")
      node.reservedBalance(bob).keys should not contain "WAVES"

      val aliceOrderId = node
        .placeOrder(
          sender = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          fee = 1920L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id
      Array(bobOrderId, aliceOrderId)
        .foreach(orderId => node.waitOrderStatus(wavesBtcPair, orderId, "Filled"))
      Array(bobOrderId, aliceOrderId)
        .foreach(orderId => node.waitOrderInBlockchain(orderId))
      node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 50150L)
      node.assertAssetBalance(alice.toAddress.toString, BtcId.toString, aliceBtcBalance + 50000L)
      node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - 1920L)
      node.assertAssetBalance(matcher.toAddress.toString, EthId.toString, matcherEthBalance + 1920L)
      node.assertAssetBalance(matcher.toAddress.toString, BtcId.toString, matcherBtcBalance + 150L)
      node.assertBalances(bob.toAddress.toString, bobWavesBalance + 1.waves)
      node.assertBalances(alice.toAddress.toString, aliceWavesBalance - 1.waves)
      Array(BtcId, EthId)
        .foreach(assetId => node.deleteRate(IssuedAsset(assetId)))
    }

    "are partial filled" in {
      val bobBtcBalance     = node.assetBalance(bob.toAddress.toString, BtcId.toString).balance
      val aliceBtcBalance   = node.assetBalance(alice.toAddress.toString, BtcId.toString).balance
      val aliceEthBalance   = node.assetBalance(alice.toAddress.toString, EthId.toString).balance
      val matcherEthBalance = node.assetBalance(matcher.toAddress.toString, EthId.toString).balance

      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
      val bobOrderId = node
        .placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 150L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(BtcId)
        )
        .message
        .id
      val aliceOrderId = node
        .placeOrder(
          sender = alice,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 2.waves,
          price = 50000L,
          fee = 1920L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id

      Map(bobOrderId -> "Filled", aliceOrderId -> "PartiallyFilled")
        .foreach(order => node.waitOrderStatus(wavesBtcPair, order._1, order._2, 500.millis))
      Array(bobOrderId, aliceOrderId)
        .foreach(orderId => node.waitOrderInBlockchain(orderId))

      node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 50150L)
      node.assertAssetBalance(alice.toAddress.toString, BtcId.toString, aliceBtcBalance + 50000L)
      node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - 960L)
      node.assertAssetBalance(matcher.toAddress.toString, EthId.toString, matcherEthBalance + 960L)

      node.cancelAllOrders(alice)
      Array(BtcId, EthId)
        .foreach(assetId => node.deleteRate(IssuedAsset(assetId)))
    }

    "are partial filled both" in {
      val params = Map(9.waves -> 213L, 1900.waves -> 1L, 2000.waves -> 0L)
      for ((aliceOrderAmount, aliceBalanceDiff) <- params) {
        val bobBtcBalance     = node.assetBalance(bob.toAddress.toString, BtcId.toString).balance
        val aliceBtcBalance   = node.assetBalance(alice.toAddress.toString, BtcId.toString).balance
        val aliceEthBalance   = node.assetBalance(alice.toAddress.toString, EthId.toString).balance
        val matcherEthBalance = node.assetBalance(matcher.toAddress.toString, EthId.toString).balance
        val bobWavesBalance   = node.accountBalances(bob.toAddress.toString)._1
        val aliceWavesBalance = node.accountBalances(alice.toAddress.toString)._1

        Map(BtcId -> btcRate, EthId -> ethRate)
          .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
        val bobOrderId = node
          .placeOrder(
            sender = bob,
            pair = wavesBtcPair,
            orderType = OrderType.BUY,
            amount = 1.waves,
            price = 50000L,
            fee = 150L,
            version = 3,
            matcherFeeAssetId = IssuedAsset(BtcId)
          )
          .message
          .id
        node.waitOrderStatus(wavesBtcPair, bobOrderId, "Accepted")
        node.reservedBalance(bob).keys should not contain "WAVES"

        val aliceOrderId = node
          .placeOrder(
            sender = alice,
            pair = wavesBtcPair,
            orderType = OrderType.SELL,
            amount = aliceOrderAmount,
            price = 50000L,
            fee = 1920L,
            version = 3,
            matcherFeeAssetId = IssuedAsset(EthId)
          )
          .message
          .id

        Map(bobOrderId -> "Filled", aliceOrderId -> "PartiallyFilled")
          .foreach(order => node.waitOrderStatus(wavesBtcPair, order._1, order._2, 500.millis))
        Array(bobOrderId, aliceOrderId)
          .foreach(orderId => node.waitOrderInBlockchain(orderId))

        node.assertAssetBalance(bob.toAddress.toString, BtcId.toString, bobBtcBalance - 50150L)
        node.assertAssetBalance(alice.toAddress.toString, BtcId.toString, aliceBtcBalance + 50000L)
        node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - aliceBalanceDiff)
        node.assertAssetBalance(matcher.toAddress.toString, EthId.toString, matcherEthBalance + aliceBalanceDiff)
        node.assertBalances(bob.toAddress.toString, bobWavesBalance + 1.waves)
        node.assertBalances(alice.toAddress.toString, aliceWavesBalance - 1.waves)

        node.cancelAllOrders(alice)
        Array(BtcId, EthId)
          .foreach(assetId => node.deleteRate(IssuedAsset(assetId)))
      }
    }
  }

  "cancellation of" - {
    val btcRate = 0.0005
    val ethRate = 0.0064
    "order with non-waves fee" in {
      val assetPair  = wavesBtcPair
      val bobBalance = node.tradableBalance(bob, assetPair)
      node.upsertRate(IssuedAsset(BtcId), btcRate, expectedStatusCode = StatusCodes.Created)
      val orderId = node
        .placeOrder(
          sender = bob,
          pair = wavesBtcPair,
          orderType = OrderType.SELL,
          amount = 1.waves,
          price = 50000L,
          fee = 150L,
          version = 3: Byte,
          matcherFeeAssetId = IssuedAsset(BtcId)
        )
        .message
        .id
      node.cancelOrder(bob, assetPair, orderId).status shouldBe "OrderCanceled"
      node.reservedBalance(bob).keys.size shouldBe 0
      node.tradableBalance(bob, wavesBtcPair) shouldEqual bobBalance
      node.deleteRate(IssuedAsset(BtcId))
    }

    "partially filled order with non-waves fee" in {
      val assetPair       = wavesBtcPair
      val aliceEthBalance = node.tradableBalance(alice, ethWavesPair)(EthId.toString)
      Map(BtcId -> btcRate, EthId -> ethRate)
        .foreach(asset => node.upsertRate(IssuedAsset(asset._1), asset._2, expectedStatusCode = StatusCodes.Created))
      val bobOrderId = node
        .placeOrder(
          sender = bob,
          pair = assetPair,
          orderType = OrderType.BUY,
          amount = 1.waves,
          price = 50000L,
          fee = 150L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(BtcId)
        )
        .message
        .id
      val aliceOrderId = node
        .placeOrder(
          sender = alice,
          pair = assetPair,
          orderType = OrderType.SELL,
          amount = 2.waves,
          price = 50000L,
          fee = 1920L,
          version = 3,
          matcherFeeAssetId = IssuedAsset(EthId)
        )
        .message
        .id
      Array(bobOrderId, aliceOrderId)
        .foreach(orderId => node.waitOrderInBlockchain(orderId))
      node.cancelOrder(alice, assetPair, aliceOrderId).status shouldBe "OrderCanceled"
      node.reservedBalance(alice).keys.size shouldBe 0
      node.assertAssetBalance(alice.toAddress.toString, EthId.toString, aliceEthBalance - 960L)
      Array(BtcId, EthId)
        .foreach(assetId => node.deleteRate(IssuedAsset(assetId)))
    }
  }
}
