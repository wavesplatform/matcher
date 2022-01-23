package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.error.FeeNotEnough

final class OrderFeeDiscountTestSuite extends OrderFeeBaseTestSuite {

  "OrderFeeDiscountTestSuite" - {
    val btcRate = 0.001
    val usdRate = 0.02

    "(dynamic settings) should work without discount" in {
      upsertAssetRate(btc -> btcRate)
      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 1.usd, matcherFee = 0.000001.btc, feeAsset = btc)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.000003 $BtcId as fee for this order, but given 0.000001 $BtcId"
      )

      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 1.usd, matcherFee = 0.000003.btc, feeAsset = btc)
      dex1.api.place(aliceOrder)
      dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 1.usd))
      waitForOrderAtNode(aliceOrder)
      wavesNode1.api.balance(alice, btc) shouldBe aliceBtcBalance - 0.000003.btc
    }

    "(dynamic settings) should apply discount" in {
      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          "waves.dex.order-fee.-1.composite.discount.value = 50"
        ).withFallback(dexInitialSuiteConfig)
      )

      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 1.usd, matcherFee = 0.0000014.btc, feeAsset = btc)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.0000015 $BtcId as fee for this order, but given 0.0000014 $BtcId"
      )

      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 1.waves, 1.usd, matcherFee = 0.0000015.btc, feeAsset = btc)
      dex1.api.place(aliceOrder)
      dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 1.waves, 1.usd))
      waitForOrderAtNode(aliceOrder)
      wavesNode1.api.balance(alice, btc) shouldBe aliceBtcBalance - 0.0000015.btc
    }

    "(percent settings [amount]) should work without discount" in {
      upsertAssetRate(btc -> btcRate)
      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          """
            |waves.dex.order-fee.-1.composite {
            |  default.mode = percent
            |  discount.value = 0
            |}
            |""".stripMargin
        ).withFallback(dexInitialSuiteConfig)
      )

      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.002.btc, feeAsset = btc)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.003 $BtcId as fee for this order, but given 0.002 $BtcId"
      )

      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.003.btc, feeAsset = btc)
      dex1.api.place(aliceOrder)
      dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 1.usd, matcherFee = 0.03.btc, feeAsset = btc))
      waitForOrderAtNode(aliceOrder)
      wavesNode1.api.balance(alice, btc) shouldBe aliceBtcBalance - 0.003.btc
    }

    "(percent settings [amount]) should apply discount" in {
      upsertAssetRate(btc -> btcRate)
      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          """
            |waves.dex.order-fee.-1.composite {
            |  default.mode = percent
            |  discount.value = 50
            |}
            |""".stripMargin
        ).withFallback(dexInitialSuiteConfig)
      )

      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.0014.btc, feeAsset = btc)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.0015 $BtcId as fee for this order, but given 0.0014 $BtcId"
      )

      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.0015.btc, feeAsset = btc)
      dex1.api.place(aliceOrder)
      dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 1.usd, matcherFee = 0.0015.btc, feeAsset = btc))
      waitForOrderAtNode(aliceOrder)
      wavesNode1.api.balance(alice, btc) shouldBe aliceBtcBalance - 0.0015.btc
    }

    "(percent settings [price]) should work without discount" in {
      upsertAssetRate(btc -> btcRate, usd -> usdRate)
      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          """
            |waves.dex.order-fee.-1.composite {
            |  default {
            |    mode = percent
            |    percent.asset-type = price
            |  }
            |  discount.value = 0
            |}
            |""".stripMargin
        ).withFallback(dexInitialSuiteConfig)
      )

      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.14.btc, feeAsset = btc)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.15 $BtcId as fee for this order, but given 0.14 $BtcId"
      )

      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.15.btc, feeAsset = btc)
      dex1.api.place(aliceOrder)
      dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 1.usd, matcherFee = 0.15.btc, feeAsset = btc))
      waitForOrderAtNode(aliceOrder)
      wavesNode1.api.balance(alice, btc) shouldBe aliceBtcBalance - 0.15.btc
    }

    "(percent settings [price]) should apply discount" in {
      upsertAssetRate(btc -> btcRate, usd -> usdRate)
      dex1.restartWithNewSuiteConfig(
        ConfigFactory.parseString(
          """
            |waves.dex.order-fee.-1.composite {
            |  default {
            |    mode = percent
            |    percent.asset-type = price
            |  }
            |  discount.value = 50
            |}
            |""".stripMargin
        ).withFallback(dexInitialSuiteConfig)
      )

      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.074.btc, feeAsset = btc)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.075 $BtcId as fee for this order, but given 0.074 $BtcId"
      )

      val aliceBtcBalance = wavesNode1.api.balance(alice, btc)
      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 100.waves, 1.usd, matcherFee = 0.075.btc, feeAsset = btc)
      dex1.api.place(aliceOrder)
      dex1.api.place(mkOrder(bob, wavesUsdPair, OrderType.SELL, 100.waves, 1.usd, matcherFee = 0.075.btc, feeAsset = btc))
      waitForOrderAtNode(aliceOrder)
      wavesNode1.api.balance(alice, btc) shouldBe aliceBtcBalance - 0.075.btc
    }
  }

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    broadcastAndAwait(
      mkTransfer(bob, alice, 100.btc, btc),
      mkTransfer(alice, bob, 100.usd, usd)
    )
    dex1.start()
  }

  override protected def dexInitialSuiteConfig: Config =
    ConfigFactory.parseString(
      s"""
         |waves.dex {
         |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
         |  allowed-order-versions = [1, 2, 3]
         |  order-fee.-1 {
         |    mode = composite
         |    composite {
         |      default {
         |        mode = dynamic
         |        dynamic {
         |          base-maker-fee = $matcherFee
         |          base-taker-fee = $matcherFee
         |        }
         |        percent {
         |          asset-type = amount
         |          min-fee = 3
         |          min-fee-in-waves = $percentMinFeeInWaves
         |        }
         |      }
         |      discount {
         |        asset = "$BtcId"
         |        value = 0
         |      }
         |    }
         |  }
         |}
       """.stripMargin
    )

}
