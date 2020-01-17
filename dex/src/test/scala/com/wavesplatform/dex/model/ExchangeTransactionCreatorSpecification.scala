package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.asset.Asset.Waves
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.error.ValidationError
import com.wavesplatform.dex.domain.model.Denormalization
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.order.OrderType
import com.wavesplatform.dex.domain.order.OrderType.{BUY, SELL}
import com.wavesplatform.dex.domain.transaction.{ExchangeTransaction, ExchangeTransactionV1, ExchangeTransactionV2}
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.dex.settings.MatcherSettings
import com.wavesplatform.dex.settings.OrderFeeSettings.DynamicSettings
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.concurrent.ExecutionContext.Implicits.global

class ExchangeTransactionCreatorSpecification
    extends AnyWordSpec
    with Matchers
    with MatcherSpecBase
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink
    with TableDrivenPropertyChecks {

  private def getExchangeTransactionCreator(hasMatcherScript: Boolean = false,
                                            hasAssetScripts: Asset => Boolean = _ => false,
                                            matcherSettings: MatcherSettings = matcherSettings): ExchangeTransactionCreator = {
    new ExchangeTransactionCreator(MatcherAccount, matcherSettings, hasMatcherScript, hasAssetScripts)
  }

  "ExchangeTransactionCreator" should {
    "create an ExchangeTransactionV1" in {

      val counter   = buy(wavesBtcPair, 100000, 0.0008, matcherFee = Some(2000L))
      val submitted = sell(wavesBtcPair, 100000, 0.0007, matcherFee = Some(1000L))

      val tc = getExchangeTransactionCreator()
      val oe = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis)

      tc.createTransaction(oe).explicitGet() shouldBe a[ExchangeTransactionV1]
    }

    "create an ExchangeTransactionV2" in {

      val counter   = buy(wavesBtcPair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
      val submitted = sell(wavesBtcPair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

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

    "create transactions with correct buyMatcherFee/sellMatcherFee for expensive feeAsset" when {

      val tc = getExchangeTransactionCreator()

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

    "charge zero fee from maker and doubled fee from taker" in {

      val ms = matcherSettings.copy(orderFee = DynamicSettings(0.003.waves, zeroMakerDoubleTaker = true))
      val tc = getExchangeTransactionCreator(matcherSettings = ms)

      def limit(amount: Long, orderType: OrderType, fee: Long = 0.006.waves, feeAsset: Asset = Waves): LimitOrder =
        LimitOrder(createOrder(wavesUsdPair, orderType, amount, 3.00, fee, feeAsset = Waves))

      def market(amount: Long, orderType: OrderType, fee: Long = 0.006.waves, feeAsset: Asset = Waves): MarketOrder =
        MarketOrder(createOrder(wavesUsdPair, orderType, amount, 3.00, fee, feeAsset = Waves), _ => Long.MaxValue)

      def createTx(s: AcceptedOrder, c: LimitOrder): Either[ValidationError, ExchangeTransaction] =
        tc.createTransaction(OrderExecuted(s, c, System.currentTimeMillis, zeroMakerFee = true))

      // format: off
      forAll(
        Table(
          ("maker amount", "taker amount", "is taker market", "taker fee"),
          /** Limit orders */
          (1.waves,  1.waves,  false, 0.006.waves),      // symmetric
          (10.waves, 1.waves,  false, 0.006.waves),      //    big maker - little taker
          (1.waves,  10.waves, false, 0.006.waves / 10), // little maker -    big taker
          /** Market orders */
          (1.waves,  1.waves,  true, 0.006.waves),      // symmetric
          (10.waves, 1.waves,  true, 0.006.waves),      //    big maker - little taker
          (1.waves,  10.waves, true, 0.006.waves / 10)  // little maker -    big taker
        )
        // format: on
      ) { (makerAmount: Long, takerAmount: Long, isTakerMarket: Boolean, takerFee: Long) =>
        val maker = limit(makerAmount, SELL)
        val taker = if (isTakerMarket) market(takerAmount, BUY) else limit(takerAmount, BUY)
        val tx    = createTx(taker, maker)

        tx shouldBe 'right

        withClue(s"maker: $maker, taker: $taker; taker fee = $takerFee\n") {
          tx.explicitGet().sellMatcherFee shouldBe 0L
          tx.explicitGet().buyMatcherFee shouldBe takerFee
        }
      }
    }
  }
}
