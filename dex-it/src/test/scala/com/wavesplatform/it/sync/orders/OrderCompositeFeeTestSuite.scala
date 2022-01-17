package com.wavesplatform.it.sync.orders

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus
import com.wavesplatform.dex.api.http.entities.HttpOrderStatus.Status
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
  }

  private def multiplyAmountByDouble(a: Long, d: Double): Long =
    (BigDecimal(a) * d).setScale(0, RoundingMode.HALF_UP).toLong

  override protected def dexInitialSuiteConfig: Config = ConfigFactory.parseString(
    s"""
       |waves.dex {
       |  price-assets = [ "$UsdId", "$BtcId", "WAVES" ]
       |  allowed-order-versions = [1, 2, 3]
       |  order-fee.-1 {
       |    mode = composite
       |    composite {
       |      custom {
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
       |    }
       |  }
       |}
       """.stripMargin
  )

  override protected def beforeAll(): Unit = {
    wavesNode1.start()
    broadcastAndAwait(IssueUsdTx, IssueBtcTx)
    broadcastAndAwait(mkTransfer(bob, alice, 100.btc, btc))
    dex1.start()
  }

}
