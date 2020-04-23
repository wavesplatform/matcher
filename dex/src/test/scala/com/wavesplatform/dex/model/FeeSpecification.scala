package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.settings.OrderFeeSettings
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class FeeSpecification
    extends AnyWordSpec
    with Matchers
    with MatcherSpecBase
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  "Fee.getMakerTakerFee should calculate correct buy/sell matcher fees" when {
    "expensive feeAsset" when {

      def test(submittedType: OrderType, submittedAmount: Long, submittedFee: Long, orderVersion: Int)(countersAmounts: Long*)(
          expectedSubmittedMatcherFees: Long*): Unit = {

        require(countersAmounts.length == expectedSubmittedMatcherFees.length)

        val submittedOrder = LimitOrder(
          createOrder(
            wavesBtcPair,
            submittedType,
            submittedAmount,
            0.00011131,
            matcherFee = submittedFee,
            feeAsset = mkAssetId("Very expensive asset"),
            version = orderVersion.toByte
          )
        )

        val counterOrders = countersAmounts.map { amount =>
          LimitOrder(createOrder(wavesBtcPair, submittedType.opposite, amount, 0.00011131))
        }

        val submittedDenormalizedAmount = Denormalization.denormalizeAmountAndFee(submittedOrder.amount, 8)
        val denormalizedCounterAmounts  = countersAmounts.map(Denormalization.denormalizeAmountAndFee(_, 8))

        // taker / maker -> maker / taker
        val getFee = Fee.getMakerTakerFee(OrderFeeSettings.DynamicSettings(0.003.waves, 0.006.waves)) _

        s"S: $submittedType $submittedDenormalizedAmount, C: ${denormalizedCounterAmounts.mkString("[", ", ", "]")}" in {
          counterOrders
            .zip(expectedSubmittedMatcherFees)
            .zipWithIndex
            .foldLeft[AcceptedOrder](submittedOrder) {
              case (submitted, ((counter, expectedMatcherFee), i)) =>
                val (makerFee, takerFee) = getFee(submitted, counter)
                val (buyMatcherFee, sellMatcherFee) =
                  if (counter.isBuyOrder) (makerFee, takerFee)
                  else (takerFee, makerFee)

                val counterAmount = Denormalization.denormalizeAmountAndFee(counter.order.amount, 8)
                withClue(s"C($i): ${counter.order.orderType} $counterAmount:\n") {
                  if (submittedType == BUY) buyMatcherFee shouldBe expectedMatcherFee
                  else sellMatcherFee shouldBe expectedMatcherFee
                }

                OrderExecuted(submitted, counter, submitted.order.timestamp, makerFee, takerFee).submittedRemaining
            }
        }
      }

      /**
        * Consider the situation, when matcherFeeAsset is very expensive, that is 1 the smallest part of it
        * (like 1 satoshi for BTC) costs at least 0.003 Waves. This means that 1 fraction of this asset
        * is enough to meet matcher's fee requirements (DynamicSettings mode, base fee = 0.003 Waves)
        *
        * In case of partial filling of the submitted order (with fee = 1 fraction of the expensive asset)
        * Fee.getMakerTakerFee has to correctly calculate buyMatcherFee/sellMatcherFee. They should have non-zero values
        * after the first execution.
        *
        * But only for orders with version >= 3, because there is a proportional checks of spent fee for older versions.
        */
      (1 to 2).foreach { v =>
        s"submitted order version is $v" when {
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(100.waves)(1)
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(99.99999999.waves)(0)
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(50.waves, 50.waves)(0, 0)
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(1.waves, 120.waves)(0, 0)
          test(SELL, 100.waves, submittedFee = 5, orderVersion = v)(2.waves, 500.waves)(0, 4)
          test(SELL, 100.waves, submittedFee = 5, orderVersion = v)(2.waves, 50.waves)(0, 2)
        }
      }

      (3 to 3).foreach { v =>
        s"submitted order version is $v" when {
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(100.waves)(1)
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(99.99999999.waves)(1)
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(50.waves, 50.waves)(1, 0)
          test(BUY, 100.waves, submittedFee = 1, orderVersion = v)(1.waves, 120.waves)(1, 0)
          test(SELL, 100.waves, submittedFee = 5, orderVersion = v)(2.waves, 500.waves)(1, 4)
          test(SELL, 100.waves, submittedFee = 5, orderVersion = v)(2.waves, 50.waves)(1, 2)
        }
      }
    }

    "submitted order v3 " when List(1, 2).foreach { counterVersion =>
      s"counter order v$counterVersion" in {
        val counter   = LimitOrder(createOrder(wavesUsdPair, BUY, amount = 88947718687647L, price = 934300L, matcherFee = 300000L, version = 2.toByte))
        val submitted = LimitOrder(createOrder(wavesUsdPair, SELL, amount = 50000000L, price = 932500L, matcherFee = 300000L, version = 3.toByte))

        val feeSettings          = DynamicSettings.symmetric(300000L)
        val (makerFee, takerFee) = Fee.getMakerTakerFee(feeSettings)(submitted, counter)

        takerFee shouldBe 0.003.waves
        makerFee shouldBe 0L
      }
    }
  }
}
