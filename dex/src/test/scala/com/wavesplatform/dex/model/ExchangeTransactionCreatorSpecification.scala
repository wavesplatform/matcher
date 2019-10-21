package com.wavesplatform.dex.model

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.MatcherTestData
import com.wavesplatform.dex.model.Events.OrderExecuted
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.OrderType._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV1, ExchangeTransactionV2, OrderType}
import com.wavesplatform.{NoShrink, crypto}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ExchangeTransactionCreatorSpecification
    extends WordSpec
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val pair = AssetPair(Waves, mkAssetId("BTC"))

  "ExchangeTransactionCreator" when {
    "SmartAccountTrading hasn't been activated yet" should {
      "create an ExchangeTransactionV1" in {

        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L))
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L))

        val tc = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, false, _ => false, _ => false)
        val oe = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis)

        tc.createTransaction(oe).explicitGet() shouldBe a[ExchangeTransactionV1]
      }

      "return an error" when {
        List((1, 2), (2, 1), (2, 2)).foreach {
          case (buyVersion, sellVersion) =>
            s"buyV$buyVersion and sellV$sellVersion" in {

              val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = buyVersion.toByte)
              val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = sellVersion.toByte)

              val tc = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, false, _ => false, _ => false)
              val oe = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis)

              tc.createTransaction(oe) should produce("Smart Account Trading feature has not been activated yet")
            }
        }
      }
    }

    "SmartAccountTrading has been activated" should {
      "create an ExchangeTransactionV2" in {

        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

        val tc = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, false, _ => false, _ == BlockchainFeatures.SmartAccountTrading.id)
        val oe = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis)

        tc.createTransaction(oe).explicitGet() shouldBe a[ExchangeTransactionV2]
      }
    }
  }

  "ExchangeTransactionCreator" should {
    "calculate fees in exchange transaction which are equal to matcher fees in fully matched orders" in {
      val preconditions = for { ((_, buyOrder), (_, sellOrder)) <- orderV3PairGenerator } yield (buyOrder, sellOrder)

      forAll(preconditions) {
        case (buyOrder, sellOrder) =>
          val tc = new ExchangeTransactionCreator(MatcherAccount,
                                                  matcherSettings,
                                                  false,
                                                  _ => false,
                                                  Set(BlockchainFeatures.OrderV3, BlockchainFeatures.SmartAccountTrading).map(_.id).contains)
          val oe = OrderExecuted(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis)
          val tx = tc.createTransaction(oe).explicitGet()

          tx.buyMatcherFee shouldBe buyOrder.matcherFee
          tx.sellMatcherFee shouldBe sellOrder.matcherFee
      }
    }

    "create valid exchange transaction when orders are matched partially" in {
      import com.wavesplatform.transaction.assets.exchange.OrderOps._

      val preconditions = for { ((_, buyOrder), (senderSell, sellOrder)) <- orderV3PairGenerator } yield {
        val sellOrderWithUpdatedAmount = sellOrder.updateAmount(sellOrder.amount / 2)
        val newSignature               = crypto.sign(senderSell, sellOrderWithUpdatedAmount.bodyBytes())
        val correctedSellOrder         = sellOrderWithUpdatedAmount.updateProofs(Proofs(Seq(ByteStr(newSignature))))

        (buyOrder, correctedSellOrder)
      }

      forAll(preconditions) {
        case (buyOrder, sellOrder) =>
          val activeSmartAccountTrading =
            if (buyOrder.version == 1 && sellOrder.version == 1) Map.empty[Short, Boolean]
            else Map(BlockchainFeatures.SmartAccountTrading.id -> true)

          val tc = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, false, _ => false, activeSmartAccountTrading.getOrElse(_, false))
          val oe = OrderExecuted(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis)
          val tx = tc.createTransaction(oe)

          tx shouldBe 'right
      }
    }

    "create transactions with correct buyMatcherFee/sellMatcherFee for expensive matcherFeeAsset" in {

      val etc = new ExchangeTransactionCreator(MatcherAccount,
                                               matcherSettings,
                                               false,
                                               _ => false,
                                               Set(BlockchainFeatures.OrderV3, BlockchainFeatures.SmartAccountTrading).map(_.id).contains)

      def test(submittedType: OrderType, submittedAmount: Long, submittedFee: Long)(countersAmounts: Long*)(expectedMatcherFees: Long*): Assertion = {

        require(countersAmounts.length == expectedMatcherFees.length)

        val submittedOrder = LimitOrder(
          createOrder(pairWavesBtc,
                      submittedType,
                      submittedAmount,
                      0.00011131,
                      matcherFee = submittedFee,
                      matcherFeeAsset = mkAssetId("Very expensive asset"))
        )

        val counterOrders = countersAmounts.map { amount =>
          LimitOrder(createOrder(pairWavesBtc, submittedType.opposite, amount, 0.00011131))
        }

        counterOrders
          .zip(expectedMatcherFees)
          .foldLeft[(AcceptedOrder, Assertion)](submittedOrder -> Succeeded) {
            case ((submitted, _), (counter, expectedMatcherFee)) =>
              val oe = OrderExecuted(submitted, counter, System.currentTimeMillis)
              val tx = etc.createTransaction(oe).explicitGet()
              val rs = if (submittedType == BUY) tx.buyMatcherFee shouldBe expectedMatcherFee else tx.sellMatcherFee shouldBe expectedMatcherFee

              oe.submittedRemaining -> rs
          }
          ._2
      }

      /**
        * Consider the situation, when matcherFeeAsset is very expensive, that is 1 the smallest part of it
        * (like 1 satoshi for BTC) costs at least 0.003 Waves. This means that 1 fraction of this asset
        * is enough to meet matcher's fee requirements (DynamicSettings mode, base fee = 0.003 Waves)
        *
        * In case of partial filling of the submitted order (with fee = 1 fraction of the expensive asset)
        * ExchangeTransactionCreator has to correctly calculate buyMatcherFee/sellMatcherFee. They should have non-zero values
        * after the first execution
        */
      withClue("S: BUY 100 (fee = 1), C: SELL 100, buyMatcherFee should be 1:\n") {
        test(BUY, 100.waves, 1)(100.waves)(1)
      }

      withClue("S: BUY 100 (fee = 1), C: SELL 99.99999999, buyMatcherFee should be 1:\n") {
        test(BUY, 100.waves, 1)(99.99999999.waves)(1)
      }

      withClue("S: BUY 100 (fee = 1), C1: SELL 50, C2: SELL 50, buyMatcherFee should be 1, 0:\n") {
        test(BUY, 100.waves, 1)(50.waves, 50.waves)(1, 0)
      }

      withClue("S: BUY 100 (fee = 1), C1: SELL 1, C2: SELL 120, buyMatcherFee should be 1, 0:\n") {
        test(BUY, 100.waves, 1)(1.waves, 120.waves)(1, 0)
      }

      withClue("S: SELL 100 (fee = 1), C1: BUY 2, C2: BUY 500, sellMatcherFee should be 1, 4:\n") {
        test(SELL, 100.waves, 5)(2.waves, 500.waves)(1, 4)
      }

      withClue("S: SELL 100 (fee = 1), C1: BUY 2, C2: BUY 50, sellMatcherFee should be 1, 2:\n") {
        test(SELL, 100.waves, 5)(2.waves, 50.waves)(1, 2)
      }
    }
  }
}
