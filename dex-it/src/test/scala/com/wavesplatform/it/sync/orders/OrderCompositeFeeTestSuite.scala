package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpCalculatedFeeResponse.CalculatedFee
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.error.FeeNotEnough

import scala.math.BigDecimal.RoundingMode

final class OrderCompositeFeeTestSuite extends OrderFeeBaseTestSuite {

  private val baseFee = 300000L

  "OrderCompositeFeeTestSuite" - {

    "should be able to place order for pair with default (dynamic) fee settings" in {
      placeAndAwaitAtDex(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1.usd, baseFee)
      ).status shouldBe HttpOrderStatus.Status.Accepted

      dex1.tryApi.place(
        mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1.usd, baseFee - 1)
      ) should failWith(
        FeeNotEnough.code,
        s"Required 0.003 WAVES as fee for this order, but given 0.00299999 WAVES"
      )

      dex1.api.cancelAllInOrderBookWithKey(wavesUsdPair)
    }

    "should be able to place order for pair with custom (percent) fee settings" in {
      val requiredFee = multiplyAmountByDouble(10.waves, 0.02)
      placeAndAwaitAtDex(
        mkOrder(alice, wavesBtcPair, OrderType.BUY, 10.waves, 1.btc, requiredFee)
      )

      dex1.tryApi.place(
        mkOrder(alice, wavesBtcPair, OrderType.BUY, 10.waves, 1.btc, requiredFee - 1)
      ) should failWith(
        FeeNotEnough.code,
        "Required 0.2 WAVES as fee for this order, but given 0.19999999 WAVES"
      )

      dex1.api.cancelAllInOrderBookWithKey(wavesBtcPair)
    }

    "custom (percent) fee after match should be calculated properly" in {
      val aliceBalance1 = wavesNode1.api.wavesBalance(alice)
      val bobBalance1 = wavesNode1.api.wavesBalance(bob)

      val aliceOrder = mkOrder(alice, wavesBtcPair, OrderType.BUY, 10.waves, 1.btc, multiplyAmountByDouble(10.waves, 0.02))
      placeAndAwaitAtDex(aliceOrder)
      val bobOrder = mkOrder(bob, wavesBtcPair, OrderType.SELL, 5.waves, 1.btc, multiplyAmountByDouble(5.waves, 0.02))
      placeAndAwaitAtDex(bobOrder, expectedStatus = Status.Filled)
      waitForOrderAtNode(bobOrder)

      val aliceBalance2 = wavesNode1.api.wavesBalance(alice)
      val bobBalance2 = wavesNode1.api.wavesBalance(bob)

      val expectedSpentFee = multiplyAmountByDouble(5.waves, 0.02)
      aliceBalance2 - aliceBalance1 shouldBe 5.waves - expectedSpentFee
      bobBalance2 - bobBalance1 shouldBe -5.waves - expectedSpentFee

      dex1.api.cancelAllInOrderBookWithKey(wavesBtcPair)
    }

    "default (dynamic) fee after match should be calculated properly" in {
      val aliceBalance1 = wavesNode1.api.wavesBalance(alice)
      val bobBalance1 = wavesNode1.api.wavesBalance(bob)

      val aliceOrder = mkOrder(alice, wavesUsdPair, OrderType.BUY, 10.waves, 1.usd, baseFee)
      placeAndAwaitAtDex(aliceOrder)
      val bobOrder = mkOrder(bob, wavesUsdPair, OrderType.SELL, 5.waves, 1.usd, baseFee)
      placeAndAwaitAtDex(bobOrder, expectedStatus = Status.Filled)
      waitForOrderAtNode(bobOrder)

      val aliceBalance2 = wavesNode1.api.wavesBalance(alice)
      val bobBalance2 = wavesNode1.api.wavesBalance(bob)

      aliceBalance2 - aliceBalance1 shouldBe 5.waves - baseFee / 2
      bobBalance2 - bobBalance1 shouldBe -5.waves - baseFee
    }

    "add custom fee assets, but not use it until offset" in {
      dex1.api.addCustomFeeAssets(Set(Asset.Waves, btc, usd))

      dex1.api.calculateFee(wavesUsdPair, OrderType.SELL, 10.waves, 25.usd)
        .base.value shouldBe CalculatedFee(Asset.Waves, baseFee)

      dex1.api.calculateFee(wavesBtcPair, OrderType.SELL, 10.waves, 1.btc)
        .base.value shouldBe CalculatedFee(Asset.Waves, 0.2.waves)

      dex1.api.place(mkOrder(alice, wavesUsdnPair, OrderType.SELL, 20.waves, 5.usdn, matcherFee = baseFee)) // just for offset to be reached
      dex1.api.place(mkOrder(alice, wavesUsdnPair, OrderType.SELL, 20.waves, 6.usdn, matcherFee = baseFee)) // just for offset to be reached

      dex1.api.place(mkOrder(alice, wavesBtcPair, OrderType.SELL, 10.waves, 1.btc, matcherFee = 0.04.waves))
      dex1.api.place(mkOrder(alice, wavesUsdPair, OrderType.SELL, 10.waves, 1.usd, matcherFee = 0.04.waves))

      dex1.api.getOrderHistoryByPKWithSig(alice, activeOnly = Some(true)).foreach { item =>
        dex1.api.cancelOneOrderWithKey(item.id, Some(alice.publicKey))
      }

      dex1.api.calculateFee(wavesBtcPair, OrderType.SELL, 10.waves, 1.btc)
        .base.value shouldBe CalculatedFee(Asset.Waves, 400000L)

      dex1.api.calculateFee(wavesUsdPair, OrderType.SELL, 10.waves, 1.usd)
        .base.value shouldBe CalculatedFee(Asset.Waves, 400000L)

      dex1.api.deleteCustomFeeAssets(Set(usd))

      dex1.api.place(mkOrder(alice, wavesBtcPair, OrderType.SELL, 10.waves, 1.btc, matcherFee = 0.04.waves))
      dex1.api.place(mkOrder(alice, wavesUsdPair, OrderType.SELL, 10.waves, 1.usd, matcherFee = baseFee))

      dex1.api.calculateFee(wavesBtcPair, OrderType.SELL, 10.waves, 1.btc)
        .base.value shouldBe CalculatedFee(Asset.Waves, 400000L)

      dex1.api.calculateFee(wavesUsdPair, OrderType.SELL, 10.waves, 1.usd)
        .base.value shouldBe CalculatedFee(Asset.Waves, baseFee)

    }

    "read all custom assets action from levelDB at start" in {
      (1 to 20).foreach { i =>
        val order = mkOrder(alice, wavesUsdnPair, OrderType.SELL, 1.waves, (5 + i).usdn, matcherFee = baseFee)
        dex1.api.place(order)
        dex1.api.cancelOrderById(order)
      }

      dex1.api.saveSnapshots

      dex1.restart()

      dex1.api.place(mkOrder(alice, wavesBtcPair, OrderType.SELL, 10.waves, 1.btc, matcherFee = 0.04.waves))
      dex1.api.place(mkOrder(alice, wavesUsdPair, OrderType.SELL, 10.waves, 1.usd, matcherFee = baseFee))

      dex1.api.calculateFee(wavesBtcPair, OrderType.SELL, 10.waves, 1.btc)
        .base.value shouldBe CalculatedFee(Asset.Waves, 400000L)

      dex1.api.calculateFee(wavesUsdPair, OrderType.SELL, 10.waves, 1.usd)
        .base.value shouldBe CalculatedFee(Asset.Waves, baseFee)

    }

  }

  private def multiplyAmountByDouble(a: Long, d: Double): Long =
    (BigDecimal(a) * d).setScale(0, RoundingMode.HALF_UP).toLong

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$UsdnId", "$BtcId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      custom-pairs {
       |        "WAVES-$BtcId": {
       |          mode = percent
       |          percent {
       |            asset-type = "amount"
       |            min-fee = 2
       |            min-fee-in-waves = $percentMinFeeInWaves
       |          }
       |        }
       |      }
       |      default {
       |        mode = dynamic
       |        dynamic {
       |          base-maker-fee = $baseFee
       |          base-taker-fee = $baseFee
       |        }
       |      }
       |      discount {
       |        asset = "$WctId"
       |        value = 0.1
       |      }
       |    }
       |  }
       |  order-fee.11 {
       |    mode = composite
       |    composite {
       |      dynamic-custom-assets = true
       |      custom-pairs {}
       |      custom-assets {
       |        assets = []
       |        settings {
       |           mode = "fixed"
       |           fixed {
       |              asset = "WAVES"
       |              min-fee = 400000
       |            }
       |        }
       |      }
       |      default {
       |        mode = dynamic
       |        dynamic {
       |          base-maker-fee = $baseFee
       |          base-taker-fee = $baseFee
       |        }
       |      }
       |      discount {
       |        asset = "$WctId"
       |        value = 0.1
       |      }
       |	  }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx, IssueUsdnTx, IssueWctTx)
    broadcastAndAwait(mkTransfer(bob, alice, 100.btc, btc))
    dex1.start()
    upsertAssetRate((usdn, 1.0), (wct, 1.0))
  }

}
