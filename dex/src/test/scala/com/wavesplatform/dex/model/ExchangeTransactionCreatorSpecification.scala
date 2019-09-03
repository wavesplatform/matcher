package com.wavesplatform.dex.model

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.dex.MatcherTestData
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransactionV1, ExchangeTransactionV2}
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
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV1]
      }

      "return an error" when {
        List((1, 2), (2, 1), (2, 2)).foreach {
          case (buyVersion, sellVersion) =>
            s"buyV$buyVersion and sellV$sellVersion" in {
              val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = buyVersion.toByte)
              val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = sellVersion.toByte)

              val tc = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, false, _ => false, _ => false)
              tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()) should produce(
                "Smart Account Trading feature has not been activated yet")
            }
        }
      }
    }

    "SmartAccountTrading has been activated" should {
      "create an ExchangeTransactionV2" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

        val tc = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, false, _ => false, _ == BlockchainFeatures.SmartAccountTrading.id)
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV2]
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
          val tx = tc.createTransaction(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis).explicitGet()

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
          val tx = tc.createTransaction(LimitOrder(buyOrder), LimitOrder(sellOrder), System.currentTimeMillis)

          tx shouldBe 'right
      }
    }
  }
}
