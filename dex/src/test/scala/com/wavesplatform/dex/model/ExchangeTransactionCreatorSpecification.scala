package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.asset.{Asset, AssetPair}
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.ExecutionContext.Implicits.global

class ExchangeTransactionCreatorSpecification
    extends WordSpec
    with Matchers
    with MatcherSpecBase
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val pair = AssetPair(Waves, mkAssetId("BTC"))

  private def getExchangeTransactionCreator(hasMatcherScript: Boolean = false,
                                            hasAssetScripts: Asset => Boolean = _ => false): ExchangeTransactionCreator = {
    new ExchangeTransactionCreator(MatcherAccount, matcherSettings, hasMatcherScript, hasAssetScripts)
  }

  "ExchangeTransactionCreator" should {
    "create an ExchangeTransactionV1" in {

      val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L))
      val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L))

      val tc = getExchangeTransactionCreator()
      val oe = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis)

      tc.createTransaction(oe).explicitGet() shouldBe a[ExchangeTransactionV1]
    }

    "create an ExchangeTransactionV2" in {

      val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
      val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

      val tc = getExchangeTransactionCreator()
      val oe = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis)

      tc.createTransaction(oe).explicitGet() shouldBe a[ExchangeTransactionV2]
    }
  }

  "ExchangeTransactionCreator" should {
    "calculate fees in exchange transaction which are equal to matcher fees in fully matched orders" in {
      val preconditions = for { ((_, buyOrder), (_, sellOrder)) <- orderV3PairGenerator } yield (buyOrder, sellOrder)

      forAll(preconditions) {
        case (buyOrder, sellOrder) =>
          val tc = getExchangeTransactionCreator()
          val oe = OrderExecuted(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis)
          val tx = tc.createTransaction(oe).explicitGet()

          tx.buyMatcherFee shouldBe buyOrder.matcherFee
          tx.sellMatcherFee shouldBe sellOrder.matcherFee
      }
    }

    "create valid exchange transaction when orders are matched partially" in {

      val preconditions = for { ((_, buyOrder), (senderSell, sellOrder)) <- orderV3PairGenerator } yield {
        val sellOrderWithUpdatedAmount = sellOrder.updateAmount(sellOrder.amount / 2)
        val newSignature               = crypto.sign(senderSell, sellOrderWithUpdatedAmount.bodyBytes())
        val correctedSellOrder         = sellOrderWithUpdatedAmount.updateProofs(Proofs(Seq(ByteStr(newSignature))))

        (buyOrder, correctedSellOrder)
      }

      forAll(preconditions) {
        case (buyOrder, sellOrder) =>
          val tc = getExchangeTransactionCreator()
          val oe = OrderExecuted(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis)
          val tx = tc.createTransaction(oe)

          tx shouldBe 'right
      }
    }

    "create transactions with correct buyMatcherFee/sellMatcherFee for expensive matcherFeeAsset" when {

      val tc = getExchangeTransactionCreator()

      def test(submittedType: OrderType, submittedAmount: Long, submittedFee: Long, orderVersion: Int)(countersAmounts: Long*)(
          expectedSubmittedMatcherFees: Long*): Unit = {

        require(countersAmounts.length == expectedSubmittedMatcherFees.length)

        val submittedOrder = LimitOrder(
          createOrder(
            pairWavesBtc,
            submittedType,
            submittedAmount,
            0.00011131,
            matcherFee = submittedFee,
            matcherFeeAsset = mkAssetId("Very expensive asset"),
            version = orderVersion.toByte
          )
        )

        val counterOrders = countersAmounts.map { amount =>
          LimitOrder(createOrder(pairWavesBtc, submittedType.opposite, amount, 0.00011131))
        }

        val submittedDenormalizedAmount = Denormalization.denormalizeAmountAndFee(submittedOrder.amount, 8)
        val denormalizedCounterAmounts  = countersAmounts.map(Denormalization.denormalizeAmountAndFee(_, 8))

        s"S: $submittedType $submittedDenormalizedAmount, C: ${denormalizedCounterAmounts.mkString("[", ", ", "]")}" in {
          counterOrders
            .zip(expectedSubmittedMatcherFees)
            .zipWithIndex
            .foldLeft[AcceptedOrder](submittedOrder) {
              case (submitted, ((counter, expectedMatcherFee), i)) =>
                val oe            = OrderExecuted(submitted, counter, System.currentTimeMillis)
                val tx            = tc.createTransaction(oe).explicitGet()
                val counterAmount = Denormalization.denormalizeAmountAndFee(counter.order.amount, 8)

                withClue(s"C($i): ${counter.order.orderType} $counterAmount:\n") {
                  if (submittedType == BUY) tx.buyMatcherFee shouldBe expectedMatcherFee
                  else tx.sellMatcherFee shouldBe expectedMatcherFee
                }

                oe.submittedRemaining
            }
        }
      }

      /**
        * Consider the situation, when matcherFeeAsset is very expensive, that is 1 the smallest part of it
        * (like 1 satoshi for BTC) costs at least 0.003 Waves. This means that 1 fraction of this asset
        * is enough to meet matcher's fee requirements (DynamicSettings mode, base fee = 0.003 Waves)
        *
        * In case of partial filling of the submitted order (with fee = 1 fraction of the expensive asset)
        * ExchangeTransactionCreator has to correctly calculate buyMatcherFee/sellMatcherFee. They should have non-zero values
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
  }
}
