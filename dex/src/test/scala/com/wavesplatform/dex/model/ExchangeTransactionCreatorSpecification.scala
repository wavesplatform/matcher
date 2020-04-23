package com.wavesplatform.dex.model

import com.wavesplatform.dex.domain.asset.Asset
import com.wavesplatform.dex.domain.bytes.ByteStr
import com.wavesplatform.dex.domain.crypto
import com.wavesplatform.dex.domain.crypto.Proofs
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.domain.order.OrderOps._
import com.wavesplatform.dex.domain.transaction.ExchangeTransactionV2
import com.wavesplatform.dex.domain.utils.EitherExt2
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
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
                                            hasAssetScripts: Asset => Boolean = _ => false): ExchangeTransactionCreator = {
    new ExchangeTransactionCreator(MatcherAccount, matcherSettings.exchangeTxBaseFee, hasMatcherScript, hasAssetScripts)
  }

  "ExchangeTransactionCreator" should {
    "create an ExchangeTransactionV2" when {
      (List(1, 2, 3) ++ List(1, 2, 3)).combinations(2).foreach {
        case List(counterVersion, submittedVersion) =>
          s"counterVersion=$counterVersion, submittedVersion=$submittedVersion" in {
            val counter   = buy(wavesBtcPair, 100000, 0.0008, matcherFee = Some(2000L), version = counterVersion.toByte)
            val submitted = sell(wavesBtcPair, 100000, 0.0007, matcherFee = Some(1000L), version = submittedVersion.toByte)

            val tc = getExchangeTransactionCreator()
            val oe = mkOrderExecutedRaw(submitted, counter)

            tc.createTransaction(oe).explicitGet() shouldBe a[ExchangeTransactionV2]
          }
      }
    }

    "take fee from order executed event" when {
      "orders are matched fully" in {
        val preconditions = for { ((_, buyOrder), (_, sellOrder)) <- orderV3MirrorPairGenerator } yield (buyOrder, sellOrder)
        test(preconditions)
      }

      "orders are matched partially" in {
        val preconditions = for { ((_, buyOrder), (senderSell, sellOrder)) <- orderV3MirrorPairGenerator } yield {
          val sellOrderWithUpdatedAmount = sellOrder.updateAmount(sellOrder.amount / 2)
          val newSignature               = crypto.sign(senderSell, sellOrderWithUpdatedAmount.bodyBytes())
          val correctedSellOrder         = sellOrderWithUpdatedAmount.updateProofs(Proofs(Seq(ByteStr(newSignature))))

          (buyOrder, correctedSellOrder)
        }

        test(preconditions)
      }

      def test(preconditions: Gen[(Order, Order)]) = forAll(preconditions) {
        case (buyOrder, sellOrder) =>
          val tc = getExchangeTransactionCreator()
          val oe = mkOrderExecutedRaw(buyOrder, sellOrder)
          val tx = tc.createTransaction(oe).explicitGet()

          tx.buyMatcherFee shouldBe oe.submittedExecutedFee
          tx.sellMatcherFee shouldBe oe.counterExecutedFee
      }
    }
  }
}
