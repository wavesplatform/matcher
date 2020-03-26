package com.wavesplatform.dex.model

import com.wavesplatform.dex.db.{OrderDB, WithDB}
import com.wavesplatform.dex.domain.account.KeyPair
import com.wavesplatform.dex.domain.asset.AssetPair
import com.wavesplatform.dex.domain.order.Order
import com.wavesplatform.dex.{MatcherSpecBase, NoShrink}
import org.scalacheck.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderDBSpec extends AnyFreeSpec with Matchers with WithDB with MatcherSpecBase with PropertyChecks with NoShrink {
  import OrderDBSpec._

  private def finalizedOrderInfoGen(o: Order): Gen[(Order, OrderInfo[OrderStatus.Final])] =
    for {
      filledAmount <- Gen.choose(0, o.amount)
      filledFee    <- Gen.choose(0, o.matcherFee)
      status       <- Gen.oneOf(OrderStatus.Filled(o.amount, o.matcherFee), OrderStatus.Cancelled(filledAmount, filledFee))
    } yield o -> o.toInfo(status)

  private def finalizedOrderSeqGen(orderCount: Int): Gen[(KeyPair, AssetPair, Seq[(Order, OrderInfo[OrderStatus.Final])])] =
    for {
      sender    <- accountGen
      pair      <- distinctPairGen
      orderList <- Gen.listOfN(orderCount, orderGenerator(sender, pair).flatMap(o => finalizedOrderInfoGen(o)))
    } yield (sender, pair, orderList)

  private val finalizedOrderInfoGen: Gen[(Order, OrderInfo[OrderStatus.Final])] = for {
    (o, _) <- orderGenerator
    result <- finalizedOrderInfoGen(o)
  } yield result

  private def test(f: OrderDB => Any): Any = f(OrderDB(matcherSettings.orderDb, db))

  "Default OrderDB implementation" - {
    "stores" - {
      "order" in test { odb =>
        forAll(orderGenerator) {
          case (o, _) =>
            odb.saveOrder(o)
        }
      }

      "order info for terminated orders" in test { odb =>
        forAll(finalizedOrderInfoGen) {
          case (o, oi) =>
            odb.saveOrderInfo(o.id(), o.sender, oi)
            odb.containsInfo(o.id()) shouldBe true
            odb.status(o.id()) shouldBe oi.status
        }
      }
    }

    "does not overwrite finalized info" in test { odb =>
      val dualFinalizedOrderInfoGen: Gen[(Order, OrderInfo[OrderStatus.Final], OrderInfo[OrderStatus.Final])] = for {
        (o, _)       <- orderGenerator
        filledAmount <- Gen.choose(0, o.amount)
        filledFee    <- Gen.choose(0, o.matcherFee)
        s1           <- Gen.oneOf(OrderStatus.Filled(o.amount, o.matcherFee), OrderStatus.Cancelled(filledAmount, filledFee))
        s2           <- Gen.oneOf(OrderStatus.Filled(o.amount, o.matcherFee), OrderStatus.Cancelled(filledAmount, filledFee))
      } yield
        (
          o,
          OrderInfo.v2(o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, s1, o.assetPair),
          OrderInfo.v2(o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, s2, o.assetPair),
        )

      forAll(dualFinalizedOrderInfoGen) {
        case (o, oi1, oi2) =>
          odb.saveOrderInfo(o.id(), o.sender, oi1)
          odb.saveOrderInfo(o.id(), o.sender, oi2)

          odb.status(o.id()) shouldBe oi1.status
      }
    }

    "loads finalized orders when limits are not exceeded" in test { odb =>
      forAll(finalizedOrderSeqGen(20)) {
        case (sender, pair, orders) =>
          for ((o, i) <- orders) {
            odb.saveOrder(o)
            odb.saveOrderInfo(o.id(), o.sender, i)
          }

          val tuples = odb.getFinalizedOrders(sender, Some(pair))
          tuples should contain allElementsOf orders.map { case (o, i) => o.id() -> i }
      }
    }

    "does not load more orders than limit" in {
      val settings = matcherSettings.orderDb.copy(maxOrders = 30)
      val odb      = OrderDB(settings, db)
      val paramGen = finalizedOrderSeqGen(40)

      forAll(paramGen) {
        case (sender, pair, finalized) =>
          for ((o, i) <- finalized) {
            odb.saveOrder(o)
            odb.saveOrderInfo(o.id(), o.sender, i)
          }

          val loadedOrders = odb.getFinalizedOrders(sender, Some(pair))
          loadedOrders.size shouldBe settings.maxOrders
      }
    }
  }
}

object OrderDBSpec {
  private implicit class OrderExt(val o: Order) extends AnyVal {
    def toInfo[A <: OrderStatus](status: A) =
      OrderInfo.v2[A](o.orderType, o.amount, o.price, o.matcherFee, o.feeAsset, o.timestamp, status, o.assetPair)
  }
}
